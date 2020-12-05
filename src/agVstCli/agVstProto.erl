-module(agVstProto).
-include("agVstCli.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   request/9
   , response/7
   , response/3
   , authInfo/2
]).

-spec authInfo(User :: binary(), Password :: binary()) -> ok.
authInfo(User, Password) ->
   AuthInfo = eVPack:encodeBin([1, 1000, <<"plain">>, User, Password]),
   MsgSize = erlang:byte_size(AuthInfo),
   <<(MsgSize + ?AgHeaderSize):32/integer-little-unsigned, 3:32/integer-little-unsigned, (agVstCli:getMsgId()):64/integer-little-unsigned, MsgSize:64/integer-little-unsigned, AuthInfo/binary>>.

-spec request(boolean(), pos_integer(), method(), binary(), path(), queryPars(), headers(), body(), pos_integer()) -> iolist().
request(IsSystem, MessageId, Method, DbName, Path, QueryPars, Headers, Body, VstSize) ->
   ReqBin =
      case IsSystem of
         false ->
            eVPack:encodeBin([1, 1, DbName, Method, Path, QueryPars, Headers]);

         _ ->
            eVPack:encodeBin([1, 1, <<"_system">>, Method, Path, QueryPars, Headers])
      end,

   MsgBin = <<ReqBin/binary, Body/binary>>,
   MsgSize = erlang:byte_size(MsgBin),
   case MsgSize =< VstSize of
      true ->
         ?AgWarn(tt, "IMY************** ~p ~p ~p ~p~n", [MsgSize, MessageId, MsgSize, MsgBin]),
         [<<(MsgSize + ?AgHeaderSize):32/integer-little-unsigned, 3:32/integer-little-unsigned, MessageId:64/integer-little-unsigned, MsgSize:64/integer-little-unsigned>>, MsgBin];
      _ ->
         ChunkCnt = erlang:ceil(MsgSize / VstSize),
         <<ChunkBin:VstSize/binary, LeftBin/binary>> = MsgBin,
         InitAccList = [ChunkBin, <<(VstSize + ?AgHeaderSize):32/integer-little-unsigned, ChunkCnt:31/integer-little-unsigned, 1:1/integer-little-unsigned, MessageId:64/integer-little-unsigned, MsgSize:64/integer-little-unsigned>>],
         AccList = buildChunk(2, VstSize, MessageId, MsgSize, MsgSize - VstSize, LeftBin, InitAccList),
         lists:reverse(AccList)
   end.

buildChunk(ChunkIdx, VstSize, MessageId, MsgSize, LeftSize, MsgBin, AccList) ->
   case LeftSize =< VstSize of
      true ->
         [MsgBin, <<(LeftSize + ?AgHeaderSize):32/integer-little-unsigned, ChunkIdx:31/integer-little-unsigned, 0:1/integer-little-unsigned, MessageId:64/integer-little-unsigned, MsgSize:64/integer-little-unsigned>> | AccList];
      _ ->
         <<ChunkBin:VstSize/binary, LeftBin/binary>> = MsgBin,
         NewAccList = [ChunkBin, <<(VstSize + ?AgHeaderSize):32/integer-little-unsigned, ChunkIdx:31/integer-little-unsigned, 0:1/integer-little-unsigned, MessageId:64/integer-little-unsigned, MsgSize:64/integer-little-unsigned>> | AccList],
         buildChunk(ChunkIdx + 1, VstSize, MessageId, MsgSize, LeftSize - VstSize, LeftBin, NewAccList)
   end.

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
               {PidFrom, TimerRef, ChunkCnt, MsgBuffer} = MsgCache = erlang:get(MessageId),
               if
                  IsFirst == ChunkX ->
                     agAgencyUtils:agencyReply(PidFrom, TimerRef, MessageId, LeftBuffer),
                     {?AgUndef, DoneCnt + 1};
                  IsFirst == 1 ->
                     MsgMB = erlang:setelement(?AgMBIdx, MsgCache, LeftBuffer),
                     MsgCC = erlang:setelement(?AgCCIdx, MsgMB, ChunkX),
                     erlang:put(MessageId, MsgCC),
                     {?AgUndef, DoneCnt};
                  true ->
                     case ChunkX >= ChunkCnt of
                        true ->
                           agAgencyUtils:agencyReply(PidFrom, TimerRef, MessageId, <<MsgBuffer/binary, LeftBuffer/binary>>),
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
               {PidFrom, TimerRef, ChunkCnt, MsgBuffer} = MsgCache = erlang:get(MessageId),
               <<ChunkBin:ChunkSize/binary, NextBuffer/binary>> = LeftBuffer,
               if
                  IsFirst == ChunkX ->
                     agAgencyUtils:agencyReply(PidFrom, TimerRef, MessageId, ChunkBin),
                     response(?AgUndef, DoneCnt + 1, 0, 0, 0, <<>>, NextBuffer);
                  IsFirst == 1 ->
                     MsgCache = erlang:get(MessageId),
                     MsgMB = erlang:setelement(?AgMBIdx, MsgCache, ChunkBin),
                     MsgCC = erlang:setelement(?AgCCIdx, MsgMB, ChunkX),
                     erlang:put(MessageId, MsgCC),
                     response(?AgUndef, DoneCnt, 0, 0, 0, <<>>, NextBuffer);
                  true ->
                     case ChunkX >= ChunkCnt of
                        true ->
                           agAgencyUtils:agencyReply(PidFrom, TimerRef, MessageId, ChunkBin),
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
               {PidFrom, TimerRef, ChunkCnt, MsgBuffer} = MsgCache = erlang:get(MessageId),
               if
                  IsFirst == ChunkX ->
                     agAgencyUtils:agencyReply(PidFrom, TimerRef, MessageId, LeftBuffer),
                     {?AgUndef, DoneCnt + 1};
                  IsFirst == 1 ->
                     MsgMB = erlang:setelement(?AgMBIdx, MsgCache, LeftBuffer),
                     MsgCC = erlang:setelement(?AgCCIdx, MsgMB, ChunkX),
                     erlang:put(MessageId, MsgCC),
                     {?AgUndef, DoneCnt};
                  true ->
                     case ChunkX >= ChunkCnt of
                        true ->
                           agAgencyUtils:agencyReply(PidFrom, TimerRef, MessageId, <<MsgBuffer/binary, LeftBuffer/binary>>),
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
               {PidFrom, TimerRef, ChunkCnt, MsgBuffer} = MsgCache = erlang:get(MessageId),
               <<ChunkBin:ChunkSize/binary, NextBuffer/binary>> = LeftBuffer,
               if
                  IsFirst == ChunkX ->
                     agAgencyUtils:agencyReply(PidFrom, TimerRef, MessageId, ChunkBin),
                     response(?AgUndef, DoneCnt + 1, 0, 0, 0, <<>>, NextBuffer);
                  IsFirst == 1 ->
                     MsgCache = erlang:get(MessageId),
                     MsgMB = erlang:setelement(?AgMBIdx, MsgCache, ChunkBin),
                     MsgCC = erlang:setelement(?AgCCIdx, MsgMB, ChunkX),
                     erlang:put(MessageId, MsgCC),
                     response(?AgUndef, DoneCnt, 0, 0, 0, <<>>, NextBuffer);
                  true ->
                     case ChunkX >= ChunkCnt of
                        true ->
                           agAgencyUtils:agencyReply(PidFrom, TimerRef, MessageId, ChunkBin),
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
         {PidFrom, TimerRef, ChunkCnt, MsgBuffer} = MsgCache = erlang:get(MessageId),
         if
            ChunkIdx >= ChunkCnt ->
               agAgencyUtils:agencyReply(PidFrom, TimerRef, MessageId, <<MsgBuffer/binary, NewCkBuffer/binary>>),
               {?AgUndef, DoneCnt + 1};
            true ->
               MsgMB = erlang:setelement(?AgMBIdx, MsgCache, <<MsgBuffer/binary, NewCkBuffer/binary>>),
               erlang:put(MessageId, MsgMB),
               {?AgUndef, DoneCnt}
         end;
      ByteSize < ChunkSize ->
         {?AgCBodyGoOn, DoneCnt, NewCkBuffer};
      true ->
         {PidFrom, TimerRef, ChunkCnt, MsgBuffer} = MsgCache = erlang:get(MessageId),
         <<ChunkBin:ChunkSize/binary, NextBuffer/binary>> = NewCkBuffer,
         if
            ChunkIdx >= ChunkCnt ->
               agAgencyUtils:agencyReply(PidFrom, TimerRef, MessageId, <<MsgBuffer/binary, ChunkBin/binary>>),
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
         ?AgWarn(1111, "response 1: ~p ~p ~p ~p ~p ~n", [ChunkX, IsFirst, MessageId, Length, _MessageLength]),

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
                     case ChunkX >= ChunkCnt of
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
                     case ChunkX >= ChunkCnt of
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