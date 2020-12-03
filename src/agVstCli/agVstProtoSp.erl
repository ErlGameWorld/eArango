-module(agVstProtoSp).
-include("agVstCli.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   request/7
   , response/7
]).

-spec request(boolean(), method(), binary(), path(), queryPars(), headers(), body()) -> iolist().
request(false, Method, DbName, Path, QueryPars, Headers, Body) ->
   [eVPack:encode([1, 1, DbName, Method, Path, QueryPars, Headers]), Body];
request(_, Method, _DbName, Path, QueryPars, Headers, Body) ->
   [eVPack:encode([1, 1, <<"/_db/_system">>, Method, Path, QueryPars, Headers]), Body].

-spec response(AgStatus :: pos_integer(), MessageId :: pos_integer(), ChunkIdx :: pos_integer(), ChunkSize :: pos_integer(), ChunkBuffer :: binary(), Data :: binary()) ->
   {?AgUndef, DoneCnt :: pos_integer()} |
   {?AgCHeader, DoneCnt :: pos_integer(), ChunkBuffer :: binary()} |
   {?AgCBodyStart, DoneCnt :: pos_integer(), MessageId :: pos_integer(), ChunkIdx :: pos_integer(), ChunkSize :: pos_integer(), ChunkBuffer :: binary()} |
   {?AgCBodyGoOn, DoneCnt :: pos_integer(), ChunkBuffer :: binary}.
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
                     {?AgCBodyStart, DoneCnt, MessageId, 1, ChunkSize, LeftBuffer}
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
                     MsgMB = erlang:setelement(?AgMBIdx, MsgCache, LeftBuffer),
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
                     {?AgCBodyStart, DoneCnt, MessageId, 1, ChunkSize, LeftBuffer}
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
                     MsgMB = erlang:setelement(?AgMBIdx, MsgCache, LeftBuffer),
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
            ChunkIdx < ChunkCnt ->
               MsgMB = erlang:setelement(?AgMBIdx, MsgCache, <<MsgBuffer/binary, NewCkBuffer/binary>>),
               erlang:put(MessageId, MsgMB),
               {?AgUndef, DoneCnt};
            true ->
               ?AgWarn(agVstProtocol_response_body, "there is not should come 11 ~p ~p ~n", [ByteSize, ChunkSize]),
               {error, error_bad_chunkIdx}
         end;
      ByteSize < ChunkSize ->
         {?AgCBodyGoOn, DoneCnt, NewCkBuffer};
      true ->
         {_PidFrom, _TimerRef, ChunkCnt, MsgBuffer} = MsgCache = erlang:get(MessageId),
         <<LastChunkBin:ChunkSize/binary, LeftBin/binary>> = NewCkBuffer,
         if
            ChunkIdx >= ChunkCnt ->
               <<LastChunkBin:ChunkSize/binary, LeftBin/binary>> = NewCkBuffer,
               agAgencyUtils:agencyReply(MessageId, <<MsgBuffer/binary, LastChunkBin/binary>>),
               response(?AgUndef, DoneCnt + 1, 0, 0, 0, <<>>, LeftBin);
            ChunkIdx < ChunkCnt ->
               MsgMB = erlang:setelement(?AgMBIdx, MsgCache, <<MsgBuffer/binary, LastChunkBin/binary>>),
               erlang:put(MessageId, MsgMB),
               response(?AgUndef, DoneCnt, 0, 0, 0, <<>>, LeftBin);
            true ->
               ?AgWarn(agVstProtocol_response_body, "there is not should come 11 ~p ~p ~n", [ByteSize, ChunkSize]),
               {error, error_bad_chunkIdx}
         end
   end.