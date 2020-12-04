-module(agVstProto).
-include("agVstCli.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   request/7
   , response/7
   , response/3
]).

%% IMY-todo 拼装 验证chunk
-spec authInfo() -> ok.
authInfo() ->
   ok.

%% IMY-todo 拼装 request chunk
-spec request(boolean(), method(), binary(), path(), queryPars(), headers(), body()) -> iolist().
request(false, Method, DbName, Path, QueryPars, Headers, Body) ->
   [eVPack:encode([1, 1, DbName, Method, Path, QueryPars, Headers]), Body];
request(_, Method, _DbName, Path, QueryPars, Headers, Body) ->
   [eVPack:encode([1, 1, <<"/_db/_system">>, Method, Path, QueryPars, Headers]), Body].

-spec response(AgStatus :: pos_integer(), DoneCnt :: pos_integer(), MessageId :: pos_integer(), ChunkIdx :: pos_integer(), ChunkSize :: pos_integer(), ChunkBuffer :: binary(), Data :: binary()) ->
   {?AgUndef, DoneCnt :: pos_integer()} |
   {?AgCHeader, DoneCnt :: pos_integer(), ChunkBuffer :: binary()} |
   {?AgCBodyStart, DoneCnt :: pos_integer(), MessageId :: pos_integer(), ChunkIdx :: pos_integer(), ChunkSize :: pos_integer(), ChunkBuffer :: binary()} |
   {?AgCBodyGoOn, DoneCnt :: pos_integer(), ChunkBuffer :: binary()}.
response(?AgUndef, DoneCnt, _MessageId, _ChunkIdx, _ChunkSize, _ChunkBuffer, DataBuffer) ->
   case DataBuffer of
      <<Length:32/integer-little-unsigned, ChunkX:31/integer-little-unsigned, IsFirst:1/integer-little-unsigned, MessageId:64/integer-little-unsigned, _MessageLength:64/integer-little-unsigned, LeftBuffer/binary>> ->
         ByteSize = erlang:byte_size(LeftBuffer),
         ChunkSize = Length - ?AgHeaderSize,
         if
            ByteSize == ChunkSize ->
               if
                  IsFirst == ChunkX ->
                     agAgencyUtils:agencyReply(MessageId, LeftBuffer),
                     {?AgUndef, DoneCnt + 1};
                  IsFirst == 1 ->
                     MsgCache = erlang:get(MessageId),
                     MsgMB = erlang:setelement(?AgMBIdx, MsgCache, LeftBuffer),
                     MsgCC = erlang:setelement(?AgCCIdx, MsgMB, ChunkX),
                     erlang:put(MessageId, MsgCC),
                     {?AgUndef, DoneCnt};
                  true ->
                     {_PidFrom, _TimerRef, ChunkCnt, MsgBuffer} = MsgCache = erlang:get(MessageId),
                     case ChunkX >= ChunkCnt  of
                        true ->
                           agAgencyUtils:agencyReply(MessageId, <<MsgBuffer/binary, LeftBuffer/binary>>),
                           {?AgUndef, DoneCnt + 1};
                        _ ->
                           MsgMB = erlang:setelement(?AgMBIdx, MsgCache, <<MsgBuffer/binary, LeftBuffer/binary>>),
                           erlang:put(MessageId, MsgMB),
                           {?AgUndef, DoneCnt}
                     end
               end;
            ByteSize < ChunkSize ->
               if
                  IsFirst == 1 ->
                     MsgCache = erlang:get(MessageId),
                     MsgCC = erlang:setelement(?AgCCIdx, MsgCache, ChunkX),
                     erlang:put(MessageId, MsgCC),
                     {?AgCBodyStart, DoneCnt, MessageId, 1, ChunkSize, LeftBuffer};
                  true ->
                     {?AgCBodyStart, DoneCnt, MessageId, ChunkX, ChunkSize, LeftBuffer}
               end;
            true ->
               {_PidFrom, _TimerRef, ChunkCnt, MsgBuffer} = MsgCache = erlang:get(MessageId),
               <<ChunkBin:ChunkSize/binary, NextBuffer/binary>> = LeftBuffer,
               if
                  IsFirst == ChunkX ->
                     agAgencyUtils:agencyReply(MessageId, ChunkBin),
                     response(?AgUndef, DoneCnt + 1, 0, 0, 0, <<>>, NextBuffer);
                  IsFirst == 1 ->
                     MsgCache = erlang:get(MessageId),
                     MsgMB = erlang:setelement(?AgMBIdx, MsgCache, ChunkBin),
                     MsgCC = erlang:setelement(?AgCCIdx, MsgMB, ChunkX),
                     erlang:put(MessageId, MsgCC),
                     response(?AgUndef, DoneCnt, 0, 0, 0, <<>>, NextBuffer);
                  true ->
                     {_PidFrom, _TimerRef, ChunkCnt, MsgBuffer} = MsgCache = erlang:get(MessageId),
                     case ChunkX >= ChunkCnt of
                        true ->
                           agAgencyUtils:agencyReply(MessageId, ChunkBin),
                           response(?AgUndef, DoneCnt + 1, 0, 0, 0, <<>>, NextBuffer);
                        _ ->
                           MsgMB = erlang:setelement(?AgMBIdx, MsgCache, <<MsgBuffer/binary, ChunkBin/binary>>),
                           erlang:put(MessageId, MsgMB),
                           response(?AgUndef, DoneCnt, 0, 0, 0, <<>>, NextBuffer)
                     end
               end
         end;
      _ ->
         {?AgCHeader, DoneCnt, DataBuffer}
   end;
response(?AgCHeader, DoneCnt, _MessageId, _ChunkIdx, _ChunkSize, ChunkBuffer, DataBuffer) ->
   NewDataBuffer = <<ChunkBuffer/binary, DataBuffer/binary>>,
   case NewDataBuffer of
      <<Length:32/integer-little-unsigned, ChunkX:31/integer-little-unsigned, IsFirst:1/integer-little-unsigned, MessageId:64/integer-little-unsigned, _MessageLength:64/integer-little-unsigned, LeftBuffer/binary>> ->
         ByteSize = erlang:byte_size(LeftBuffer),
         ChunkSize = Length - ?AgHeaderSize,
         if
            ByteSize == ChunkSize ->
               if
                  IsFirst == ChunkX ->
                     agAgencyUtils:agencyReply(MessageId, LeftBuffer),
                     {?AgUndef, DoneCnt + 1};
                  IsFirst == 1 ->
                     MsgCache = erlang:get(MessageId),
                     MsgMB = erlang:setelement(?AgMBIdx, MsgCache, LeftBuffer),
                     MsgCC = erlang:setelement(?AgCCIdx, MsgMB, ChunkX),
                     erlang:put(MessageId, MsgCC),
                     {?AgUndef, DoneCnt};
                  true ->
                     {_PidFrom, _TimerRef, ChunkCnt, MsgBuffer} = MsgCache = erlang:get(MessageId),
                     case ChunkX >= ChunkCnt  of
                        true ->
                           agAgencyUtils:agencyReply(MessageId, <<MsgBuffer/binary, LeftBuffer/binary>>),
                           {?AgUndef, DoneCnt + 1};
                        _ ->
                           MsgMB = erlang:setelement(?AgMBIdx, MsgCache, <<MsgBuffer/binary, LeftBuffer/binary>>),
                           erlang:put(MessageId, MsgMB),
                           {?AgUndef, DoneCnt}
                     end
               end;
            ByteSize < ChunkSize ->
               if
                  IsFirst == 1 ->
                     MsgCache = erlang:get(MessageId),
                     MsgCC = erlang:setelement(?AgCCIdx, MsgCache, ChunkX),
                     erlang:put(MessageId, MsgCC),
                     {?AgCBodyStart, DoneCnt, MessageId, 1, ChunkSize, LeftBuffer};
                  true ->
                     {?AgCBodyStart, DoneCnt, MessageId, ChunkX, ChunkSize, LeftBuffer}
               end;
            true ->
               {_PidFrom, _TimerRef, ChunkCnt, MsgBuffer} = MsgCache = erlang:get(MessageId),
               <<ChunkBin:ChunkSize/binary, NextBuffer/binary>> = LeftBuffer,
               if
                  IsFirst == ChunkX ->
                     agAgencyUtils:agencyReply(MessageId, ChunkBin),
                     response(?AgUndef, DoneCnt + 1, 0, 0, 0, <<>>, NextBuffer);
                  IsFirst == 1 ->
                     MsgCache = erlang:get(MessageId),
                     MsgMB = erlang:setelement(?AgMBIdx, MsgCache, ChunkBin),
                     MsgCC = erlang:setelement(?AgCCIdx, MsgMB, ChunkX),
                     erlang:put(MessageId, MsgCC),
                     response(?AgUndef, DoneCnt, 0, 0, 0, <<>>, NextBuffer);
                  true ->
                     {_PidFrom, _TimerRef, ChunkCnt, MsgBuffer} = MsgCache = erlang:get(MessageId),
                     case ChunkX >= ChunkCnt of
                        true ->
                           agAgencyUtils:agencyReply(MessageId, ChunkBin),
                           response(?AgUndef, DoneCnt + 1, 0, 0, 0, <<>>, NextBuffer);
                        _ ->
                           MsgMB = erlang:setelement(?AgMBIdx, MsgCache, <<MsgBuffer/binary, ChunkBin/binary>>),
                           erlang:put(MessageId, MsgMB),
                           response(?AgUndef, DoneCnt, 0, 0, 0, <<>>, NextBuffer)
                     end
               end
         end;
      _ ->
         {?AgCHeader, DoneCnt, NewDataBuffer}
   end;
response(?AgCBody, DoneCnt, MessageId, ChunkIdx, ChunkSize, ChunkBuffer, DataBuffer) ->
   NewCkBuffer = <<ChunkBuffer/binary, DataBuffer/binary>>,
   ByteSize = erlang:byte_size(NewCkBuffer),
   if
      ChunkSize == ByteSize ->
         {_PidFrom, _TimerRef, ChunkCnt, MsgBuffer} = MsgCache = erlang:get(MessageId),
         if
            ChunkIdx >= ChunkCnt ->
               agAgencyUtils:agencyReply(MessageId, <<MsgBuffer/binary, NewCkBuffer/binary>>),
               {?AgUndef, DoneCnt + 1};
            true ->
               MsgMB = erlang:setelement(?AgMBIdx, MsgCache, <<MsgBuffer/binary, NewCkBuffer/binary>>),
               erlang:put(MessageId, MsgMB),
               {?AgUndef, DoneCnt}
         end;
      ByteSize < ChunkSize ->
         {?AgCBodyGoOn, DoneCnt, NewCkBuffer};
      true ->
         {_PidFrom, _TimerRef, ChunkCnt, MsgBuffer} = MsgCache = erlang:get(MessageId),
         <<ChunkBin:ChunkSize/binary, NextBuffer/binary>> = NewCkBuffer,
         if
            ChunkIdx >= ChunkCnt ->
               agAgencyUtils:agencyReply(MessageId, <<MsgBuffer/binary, ChunkBin/binary>>),
               response(?AgUndef, DoneCnt + 1, 0, 0, 0, <<>>, NextBuffer);
            true ->
               MsgMB = erlang:setelement(?AgMBIdx, MsgCache, <<MsgBuffer/binary, ChunkBin/binary>>),
               erlang:put(MessageId, MsgMB),
               response(?AgUndef, DoneCnt, 0, 0, 0, <<>>, NextBuffer)
         end
   end.

-spec response(AgStatus :: pos_integer(), RecvState :: recvState(), DataBuffer :: binary()) ->
   {?AgMDone, MsgBin :: binary()} |
   {?AgCHeader, RecvState :: recvState()} |
   {?AgCBodyStart, RecvState :: recvState()} |
   {?AgCBodyGoOn, RecvState :: recvState()}.
response(?AgUndef, #recvState{chunkCnt = ChunkCnt, msgBuffer = MsgBuffer} = RecvState, DataBuffer) ->
   case DataBuffer of
      <<Length:32/integer-little-unsigned, ChunkX:31/integer-little-unsigned, IsFirst:1/integer-little-unsigned, MessageId:64/integer-little-unsigned, _MessageLength:64/integer-little-unsigned, LeftBuffer/binary>> ->
         ByteSize = erlang:byte_size(LeftBuffer),
         ChunkSize = Length - ?AgHeaderSize,
         if
            ByteSize == ChunkSize ->
               if
                  IsFirst == ChunkX ->
                     agAgencyUtils:agencyReply(MessageId, LeftBuffer),
                     {?AgMDone, LeftBuffer};
                  IsFirst == 1 ->
                     {?AgCDone, #recvState{revStatus = ?AgUndef, messageId = MessageId, chunkCnt = ChunkX, msgBuffer = LeftBuffer}};
                  true ->
                     case ChunkX >= ChunkCnt  of
                        true ->
                           {?AgMDone, <<MsgBuffer/binary, LeftBuffer/binary>>};
                        _ ->
                           {?AgCDone, #recvState{revStatus = ?AgUndef, messageId = MessageId, msgBuffer = <<MsgBuffer/binary, LeftBuffer/binary>>}}
                     end
               end;
            true ->
               if
                  IsFirst == 1 ->
                     {?AgCBodyStart, RecvState#recvState{revStatus = ?AgCBody, messageId = MessageId, chunkCnt = ChunkX, chunkIdx = 1, chunkSize = ChunkSize, chunkBuffer = LeftBuffer}};
                  true ->
                     {?AgCBodyStart, RecvState#recvState{revStatus = ?AgCBody, chunkIdx = ChunkX, chunkSize = ChunkSize, chunkBuffer = LeftBuffer}}
               end
         end;
      _ ->
         {?AgCHeader, RecvState#recvState{revStatus = ?AgCHeader, chunkBuffer = DataBuffer}}
   end;
response(?AgCHeader, #recvState{chunkCnt = ChunkCnt, msgBuffer = MsgBuffer, chunkBuffer = ChunkBuffer} = RecvState, DataBuffer) ->
   NewDataBuffer = <<ChunkBuffer/binary, DataBuffer/binary>>,
   case NewDataBuffer of
      <<Length:32/integer-little-unsigned, ChunkX:31/integer-little-unsigned, IsFirst:1/integer-little-unsigned, MessageId:64/integer-little-unsigned, _MessageLength:64/integer-little-unsigned, LeftBuffer/binary>> ->
         ByteSize = erlang:byte_size(LeftBuffer),
         ChunkSize = Length - ?AgHeaderSize,
         if
            ByteSize == ChunkSize ->
               if
                  IsFirst == ChunkX ->
                     agAgencyUtils:agencyReply(MessageId, LeftBuffer),
                     {?AgMDone, LeftBuffer};
                  IsFirst == 1 ->
                     {?AgCDone, #recvState{revStatus = ?AgUndef, messageId = MessageId, chunkCnt = ChunkX, msgBuffer = LeftBuffer}};
                  true ->
                     case ChunkX >= ChunkCnt  of
                        true ->
                           {?AgMDone, <<MsgBuffer/binary, LeftBuffer/binary>>};
                        _ ->
                           {?AgCDone, #recvState{revStatus = ?AgUndef, messageId = MessageId, msgBuffer = <<MsgBuffer/binary, LeftBuffer/binary>>}}
                     end
               end;
            true ->
               if
                  IsFirst == 1 ->
                     {?AgCBodyStart, RecvState#recvState{revStatus = ?AgCBody, messageId = MessageId, chunkCnt = ChunkX, chunkIdx = 1, chunkSize = ChunkSize, chunkBuffer = LeftBuffer}};
                  true ->
                     {?AgCBodyStart, RecvState#recvState{revStatus = ?AgCBody, chunkIdx = ChunkX, chunkSize = ChunkSize, chunkBuffer = LeftBuffer}}
               end
         end;
      _ ->
         {?AgCHeader, RecvState#recvState{revStatus = ?AgCHeader, chunkBuffer = NewDataBuffer}}
   end;
response(?AgCBody, #recvState{chunkCnt = ChunkCnt, msgBuffer = MsgBuffer, chunkIdx = ChunkIdx, chunkSize = ChunkSize, chunkBuffer = ChunkBuffer} = RecvState, DataBuffer) ->
   NewCkBuffer = <<ChunkBuffer/binary, DataBuffer/binary>>,
   ByteSize = erlang:byte_size(NewCkBuffer),
   if
      ChunkSize == ByteSize ->
         if
            ChunkIdx >= ChunkCnt ->
               {?AgMDone, <<MsgBuffer/binary, NewCkBuffer/binary>>};
            true ->
               {?AgCDone, RecvState#recvState{revStatus = ?AgUndef, msgBuffer = <<MsgBuffer/binary, NewCkBuffer/binary>>}}
         end;
      true ->
         {?AgCBodyGoOn, RecvState#recvState{chunkBuffer = NewCkBuffer}}
   end.