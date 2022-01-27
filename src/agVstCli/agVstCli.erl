-module(agVstCli).
-include("agVstCli.hrl").
-include("eArango.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   %% Common Request API
   callAgency/3
   , callAgency/4
   , callAgency/6
   , callAgency/7
   , callAgency/8
   , castAgency/6
   , castAgency/7
   , castAgency/8
   , castAgency/9
   , receiveReqRet/2

   , initMsgId/0
   , getMsgId/0
   , receiveTcpData/2
   , receiveSslData/2
]).


-spec callAgency(poolNameOrSocket(), method(), path()) -> term() | {error, term()}.
callAgency(PoolNameOrSocket, Method, Path) ->
   callAgency(PoolNameOrSocket, Method, Path, ?AgDefQuery, ?AgDefHeader, ?AgDefBody, false, ?AgDefTimeout).

-spec callAgency(poolNameOrSocket(), method(), path(), queryPars()) -> term() | {error, term()}.
callAgency(PoolNameOrSocket, Method, Path, QueryPars) ->
   callAgency(PoolNameOrSocket, Method, Path, QueryPars, ?AgDefHeader, ?AgDefBody, false, ?AgDefTimeout).

-spec callAgency(poolNameOrSocket(), method(), path(), queryPars(), headers(), body()) -> term() | {error, term()}.
callAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body) ->
   callAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body, false, ?AgDefTimeout).

-spec callAgency(poolNameOrSocket(), method(), path(), queryPars(), headers(), body(), boolean()) -> term() | {error, atom()}.
callAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body, IsSystem) ->
   callAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body, IsSystem, ?AgDefTimeout).

-spec callAgency(poolNameOrSocket(), method(), path(), queryPars(), headers(), body(), boolean(), timeout()) -> term() | {error, atom()}.
callAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body, IsSystem, Timeout) ->
   case castAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body, self(), IsSystem, Timeout) of
      {waitRRT, RequestId, MonitorRef} ->
         receiveReqRet(RequestId, MonitorRef);
      {error, _Reason} = Err ->
         Err;
      Ret ->
         Ret
   end.

-spec castAgency(poolNameOrSocket(), method(), path(), queryPars(), headers(), body()) -> {ok, messageId()} | {error, atom()}.
castAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body) ->
   castAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body, self(), false, ?AgDefTimeout).

-spec castAgency(poolNameOrSocket(), method(), path(), queryPars(), headers(), body(), boolean()) -> {ok, messageId()} | {error, atom()}.
castAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body, IsSystem) ->
   castAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body, self(), IsSystem, ?AgDefTimeout).

-spec castAgency(poolNameOrSocket(), method(), path(), queryPars(), headers(), body(), boolean(), timeout()) -> {ok, messageId()} | {error, atom()}.
castAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body, IsSystem, Timeout) ->
   castAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body, self(), IsSystem, Timeout).

-spec castAgency(poolNameOrSocket(), method(), path(), queryPars(), headers(), body(), pid(), boolean(), timeout()) -> {ok, messageId()} | {error, atom()}.
castAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body, Pid, IsSystem, Timeout) ->
   OverTime =
      case Timeout of
         infinity -> infinity;
         _ ->
            erlang:monotonic_time(millisecond) + Timeout
      end,
   case erlang:is_atom(PoolNameOrSocket) of
      true ->
         case agAgencyPoolMgr:getOneAgency(PoolNameOrSocket) of
            {error, pool_not_found} = Err ->
               Err;
            undefined ->
               {error, undefined_server};
            AgencyName ->
               MonitorRef = erlang:monitor(process, AgencyName),
               MessageId = getMsgId(),
               catch AgencyName ! #agReq{method = Method, path = Path, queryPars = QueryPars, headers = Headers, body = Body, messageId = MessageId, fromPid = Pid, overTime = OverTime, isSystem = IsSystem},
               {waitRRT, MessageId, MonitorRef}
         end;
      _ ->
         case eArango:getCurDbInfo(PoolNameOrSocket) of
            {DbName, VstSize, Protocol} ->
               MessageId = getMsgId(),
               Request = agVstProto:request(IsSystem, MessageId, Method, DbName, Path, QueryPars, Headers, Body, VstSize),
               case Protocol of
                  tcp ->
                     case gen_tcp:send(PoolNameOrSocket, Request) of
                        ok ->
                           receiveTcpData(#recvState{}, PoolNameOrSocket);
                        {error, Reason} = Err ->
                           ?AgWarn(castAgency, ":gen_tcp send error: ~p ~n", [Reason]),
                           eArango:disConnDb(PoolNameOrSocket),
                           Err
                     end;
                  ssl ->
                     case ssl:send(PoolNameOrSocket, Request) of
                        ok ->
                           receiveSslData(#recvState{}, PoolNameOrSocket);
                        {error, Reason} = Err ->
                           ?AgWarn(castAgency, ":ssl send error: ~p ~n", [Reason]),
                           eArango:disConnDb(PoolNameOrSocket),
                           Err
                     end
               end;
            _ ->
               {error, dbinfoNotFound}
         end
   end.

-spec receiveReqRet(messageId(), reference()) -> {StatusCode :: non_neg_integer(), Body :: map(), Headers :: map()} | {error, term()}.
receiveReqRet(RequestId, MonitorRef) ->
   receive
      #agReqRet{messageId = RequestId, reply = Reply} ->
         erlang:demonitor(MonitorRef),
         case Reply of
            {error, Err} ->
               Err;
            _ ->
               {[_1, _2, StatusCode, HeaderMap], BodyMap} = eVPack:decodeAll(Reply),
               {StatusCode, BodyMap, HeaderMap}
         end;
      {'DOWN', MonitorRef, process, _Pid, Reason} ->
         {error, {agencyDown, Reason}}
   end.

-spec receiveTcpData(recvState(), socket()) -> {ok, term(), term()} | {error, term()}.
receiveTcpData(RecvState, Socket) ->
   receive
      {tcp, Socket, DataBuffer} ->
         %% ?AgWarn(1111, "IMY************receove 1: ~p ~p ~n", [erlang:byte_size(DataBuffer), DataBuffer]),
         case agVstProto:response(element(2, RecvState), RecvState, DataBuffer) of
            {?AgMDone, MsgBin} ->
               {ok, MsgBin};
            {?AgCHeader, NewRecvState} ->
               receiveTcpData(NewRecvState, Socket);
            {?AgCBodyStart, NewRecvState} ->
               receiveTcpData(NewRecvState, Socket);
            {?AgCBodyGoOn, NewRecvState} ->
               receiveTcpData(NewRecvState, Socket)
         end;
      {tcp_closed, Socket} ->
         eArango:disConnDb(Socket),
         {error, tcp_closed};
      {tcp_error, Socket, Reason} ->
         eArango:disConnDb(Socket),
         {error, {tcp_error, Reason}}
   end.

-spec receiveSslData(recvState(), socket()) -> {ok, term(), term()} | {error, term()}.
receiveSslData(RecvState, Socket) ->
   receive
      {ssl, Socket, DataBuffer} ->
         case agVstProto:response(element(2, RecvState), RecvState, DataBuffer) of
            {?AgMDone, MsgBin} ->
               {ok, MsgBin};
            {?AgCHeader, NewRecvState} ->
               receiveSslData(NewRecvState, Socket);
            {?AgCBodyStart, NewRecvState} ->
               receiveSslData(NewRecvState, Socket);
            {?AgCBodyGoOn, NewRecvState} ->
               receiveSslData(NewRecvState, Socket)
         end;
      {ssl_closed, Socket} ->
         eArango:disConnDb(Socket),
         {error, ssl_closed};
      {ssl_error, Socket, Reason} ->
         eArango:disConnDb(Socket),
         {error, {ssl_error, Reason}}
   end.

initMsgId() ->
   case persistent_term:get(agMessageId, undefined) of
      undefined ->
         Ref = atomics:new(1, [{signed, false}]),
         InitId = rand:uniform(10000),
         atomics:put(Ref, 1, InitId),
         persistent_term:put(agMessageId, Ref);
      _ ->
         ignore
   end.

getMsgId() ->
   Ref = persistent_term:get(agMessageId, undefined),
   MessageId = atomics:add_get(Ref, 1, 1),
   if
      MessageId >= ?agMaxMessageId ->
         InitId = rand:uniform(10000),
         atomics:put(Ref, 1, InitId),
         InitId;
      true ->
         MessageId
   end.

