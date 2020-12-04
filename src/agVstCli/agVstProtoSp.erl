-module(agVstProtoSp).
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