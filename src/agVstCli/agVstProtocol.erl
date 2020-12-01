-module(agVstProtocol).
-include("agVstCli.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   request/7
   , response/3
]).

-spec request(boolean(), method(), binary(), path(), queryPars(), headers(), body()) -> iolist().
request(false, Method, DbName, Path, QueryPars, Headers, Body) ->
   [eVPack:encode([1, 1, DbName, Method, Path, QueryPars, Headers]), Body];
request(_, Method, _DbName, Path, QueryPars, Headers, Body) ->
   [eVPack:encode([1, 1, <<"/_db/_system">>, Method, Path, QueryPars, Headers]), Body].


-spec response(undefined | recvState(), binary()) -> {ok, recvState()} | error().
response(?AgCUndef, Buffer, Data) ->
   case Data of
      <<Length:32/integer-little-unsigned, ChunkX:31/integer-little-unsigned, IsFirst:1/integer-little-unsigned, MessageId:64/integer-little-unsigned, MessageLength:64/integer-little-unsigned, LeftBin/binary>> ->
         ByteSize = erlang:byte_size(LeftBin),
         ChunkSize = Length - 24,
         if
            ByteSize == ChunkSize ->
               if
                  IsFirst == ChunkX  ->
                     {?AgMDone, LeftBin};
                  IsFirst == 1 ->
                     erlang:put(MessageId, {ChunkX, LeftBin});
                  true ->
                     case erlang:get(MessageId) of
                        {ChunkX, DataBin} ->
                           {?AgMDone, <<DataBin/binary, LeftBin/binary>>};
                        {SumChunk, DataBin} ->
                           erlang:put(MessageId, {SumChunk, <<DataBin/binary, LeftBin/binary>>}),
                           {?AgCBody, Data};
                        _ ->
                           throw(error_happen)
                     end
               end;
            ByteSize < ChunkSize ->
               {?AgCBody, Data};
            true ->
               throw(error_bad_size)
            end;
      _ ->
         {?AgCHeader, Data}
   end;
response(?AgCHeader, Buffer, Data) ->
   NewData = <<Buffer/binary, Data/binary>>,
   case NewData of
      <<Length:32/integer-little-unsigned, ChunkX:31/integer-little-unsigned, IsFirst:1/integer-little-unsigned, MessageId:64/integer-little-unsigned, MessageLength:64/integer-little-unsigned, LeftBin/binary>> ->
         ByteSize = erlang:byte_size(LeftBin),
         ChunkSize = Length - 24,
         if
            ByteSize == ChunkSize ->
               if
                  IsFirst == ChunkX  ->
                     {?AgMDone, LeftBin};
                  IsFirst == 1 ->
                     erlang:put(MessageId, {ChunkX, LeftBin});
                  true ->
                     case erlang:get(MessageId) of
                        {ChunkX, DataBin} ->
                           {?AgMDone, <<DataBin/binary, LeftBin/binary>>};
                        {SumChunk, DataBin} ->
                           erlang:put(MessageId, {SumChunk, <<DataBin/binary, LeftBin/binary>>}),
                           {?AgCBody, Data};
                        _ ->
                           throw(error_happen)
                     end
               end;
            ByteSize < ChunkSize ->
               {?AgCBody, Data};
            true ->
               throw(error_bad_size)
         end;
      _ ->
         {?AgCHeader, Data}
   end;
response(?AgCUndef, Buffer, Data) ->
   case Data of
      <<Length:32/integer-little-unsigned, ChunkX:31/integer-little-unsigned, IsFirst:1/integer-little-unsigned, MessageId:64/integer-little-unsigned, MessageLength:64/integer-little-unsigned, LeftBin/binary>> ->
         ByteSize = erlang:byte_size(LeftBin),
         ChunkSize = Length - 24,
         if
            ByteSize == ChunkSize ->
               if
                  IsFirst == ChunkX  ->
                     {?AgMDone, LeftBin};
                  IsFirst == 1 ->
                     erlang:put(MessageId, {ChunkX, LeftBin});
                  true ->
                     case erlang:get(MessageId) of
                        {ChunkX, DataBin} ->
                           {?AgMDone, <<DataBin/binary, LeftBin/binary>>};
                        {SumChunk, DataBin} ->
                           erlang:put(MessageId, {SumChunk, <<DataBin/binary, LeftBin/binary>>}),
                           {?AgCBody, Data};
                        _ ->
                           throw(error_happen)
                     end
               end;
            ByteSize < ChunkSize ->
               {?AgCBody, Data};
            true ->
               throw(error_bad_size)
         end;
      _ ->
         {?AgCHeader, Data}
   end;


