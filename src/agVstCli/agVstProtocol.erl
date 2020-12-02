-module(agVstProtocol).
-include("agVstCli.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   request/7
   , response/6
]).

-spec request(boolean(), method(), binary(), path(), queryPars(), headers(), body()) -> iolist().
request(false, Method, DbName, Path, QueryPars, Headers, Body) ->
   [eVPack:encode([1, 1, DbName, Method, Path, QueryPars, Headers]), Body];
request(_, Method, _DbName, Path, QueryPars, Headers, Body) ->
   [eVPack:encode([1, 1, <<"/_db/_system">>, Method, Path, QueryPars, Headers]), Body].

-spec response(AgStatus :: pos_integer(), MessageId :: pos_integer(), ChunkIdx :: pos_integer(), ChunkSize :: pos_integer(), ChunkBuffer :: binary(), Data :: binary()) ->
   ?AgCDone |
   {?AgCHeader, ChunkBuffer :: binary()} |
   {?AgMDone, MsgBuffer :: binary()} |
   {?AgCBodyStart, MessageId :: pos_integer(), ChunkIdx :: pos_integer(), ChunkSize :: pos_integer(), ChunkBuffer :: binary()} |
   {?AgCBodyGoOn, ChunkBuffer}  |
   error().
response(?AgCUndef, _MessageId, _ChunkIdx, _ChunkSize, _ChunkBuffer, Data) ->
   case Data of
      <<Length:32/integer-little-unsigned, ChunkX:31/integer-little-unsigned, IsFirst:1/integer-little-unsigned, MessageId:64/integer-little-unsigned, _MessageLength:64/integer-little-unsigned, LeftBin/binary>> ->
         ByteSize = erlang:byte_size(LeftBin),
         ChunkSize = Length - 24,
         if
            ByteSize == ChunkSize ->
               if
                  IsFirst == ChunkX ->
                     {?AgMDone, LeftBin};
                  IsFirst == 1 ->
                     MsgCache = erlang:get(MessageId),
                     MsgMB = erlang:setelement(?AgMBIdx, MsgCache, LeftBin),
                     MsgCC = erlang:setelement(?AgCCIdx, MsgMB, ChunkX),
                     erlang:put(MessageId, MsgCC),
                     ?AgCDone;
                  true ->
                     {_PidFrom, _TimerRef, ChunkCnt, MsgBuffer} = MsgCache = erlang:get(MessageId),
                     case ChunkCnt == ChunkX of
                        true ->
                           {?AgMDone, <<MsgBuffer/binary, LeftBin/binary>>};
                        _ ->
                           MsgMB = erlang:setelement(?AgMBIdx, MsgCache, <<MsgBuffer/binary, LeftBin/binary>>),
                           erlang:put(MessageId, MsgMB),
                           ?AgCDone
                     end
               end;
            ByteSize < ChunkSize ->
               if
                  IsFirst == 1 ->
                     MsgCache = erlang:get(MessageId),
                     MsgCC = erlang:setelement(?AgCCIdx, MsgCache, ChunkX),
                     erlang:put(MessageId, MsgCC),
                     {?AgCBodyStart, MessageId, 1, ChunkSize, LeftBin};
                  true ->
                     {?AgCBodyStart, MessageId, 1, ChunkSize, LeftBin}
               end;
            true ->
               ?AgWarn(agVstProtocol_response_undef, "there is not should come ~p ~p ~p ~n", [ByteSize, ChunkSize, {Length, ChunkX, IsFirst, MessageId}]),
               {error, error_bad_size}
         end;
      _ ->
         {?AgCHeader, Data}
   end;
response(?AgCHeader, _MessageId, _ChunkIdx, _ChunkSize, ChunkBuffer, Data) ->
   NewData = <<ChunkBuffer/binary, Data/binary>>,
   case NewData of
      <<Length:32/integer-little-unsigned, ChunkX:31/integer-little-unsigned, IsFirst:1/integer-little-unsigned, MessageId:64/integer-little-unsigned, _MessageLength:64/integer-little-unsigned, LeftBin/binary>> ->
         ByteSize = erlang:byte_size(LeftBin),
         ChunkSize = Length - 24,
         if
            ByteSize == ChunkSize ->
               if
                  IsFirst == ChunkX ->
                     {?AgMDone, LeftBin};
                  IsFirst == 1 ->
                     MsgCache = erlang:get(MessageId),
                     MsgMB = erlang:setelement(?AgMBIdx, MsgCache, LeftBin),
                     MsgCC = erlang:setelement(?AgCCIdx, MsgMB, ChunkX),
                     erlang:put(MessageId, MsgCC),
                     ?AgCDone;
                  true ->
                     {_PidFrom, _TimerRef, ChunkCnt, MsgBuffer} = MsgCache = erlang:get(MessageId),
                     case ChunkCnt == ChunkX of
                        true ->
                           {?AgMDone, <<MsgBuffer/binary, LeftBin/binary>>};
                        _ ->
                           MsgMB = erlang:setelement(?AgMBIdx, MsgCache, <<MsgBuffer/binary, LeftBin/binary>>),
                           erlang:put(MessageId, MsgMB),
                           ?AgCDone
                     end
               end;
            ByteSize < ChunkSize ->
               if
                  IsFirst == 1 ->
                     MsgCache = erlang:get(MessageId),
                     MsgCC = erlang:setelement(?AgCCIdx, MsgCache, ChunkX),
                     erlang:put(MessageId, MsgCC),
                     {?AgCBodyStart, MessageId, 1, ChunkSize, LeftBin};
                  true ->
                     {?AgCBodyStart, MessageId, 1, ChunkSize, LeftBin}
               end;
            true ->
               ?AgWarn(agVstProtocol_response_undef, "there is not should come ~p ~p ~p ~n", [ByteSize, ChunkSize, {Length, ChunkX, IsFirst, MessageId}]),
               {error, error_bad_size}
         end;
      _ ->
         {?AgCHeader, Data}
   end;
response(?AgCBody, MessageId, ChunkIdx, ChunkSize, ChunkBuffer, Data) ->
   NewCkBuffer = <<ChunkBuffer/binary, Data/binary>>,
   ByteSize = erlang:byte_size(NewCkBuffer),
   if
      ChunkSize == ByteSize ->
         {_PidFrom, _TimerRef, ChunkCnt, MsgBuffer} = MsgCache = erlang:get(MessageId),
         if
            ChunkIdx == ChunkCnt ->
               {?AgMDone, <<MsgBuffer/binary, NewCkBuffer/binary>>};
            ChunkIdx < ChunkCnt ->
               MsgMB = erlang:setelement(?AgMBIdx, MsgCache, <<MsgBuffer/binary, NewCkBuffer/binary>>),
               erlang:put(MessageId, MsgMB),
               ?AgCDone;
            true ->
               ?AgWarn(agVstProtocol_response_body, "there is not should come 11 ~p ~p ~n", [ByteSize, ChunkSize]),
               {error, error_bad_chunkIdx}
         end;
      ByteSize < ChunkSize ->
         {?AgCBodyGoOn, NewCkBuffer};
      true ->
         ?AgWarn(agVstProtocol_response_body, "there is not should come 22 ~p ~p ~n", [ByteSize, ChunkSize]),
         {error, error_bad_size}
   end.


