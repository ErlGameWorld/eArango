-module(agMiscUtils).
-include("agVstCli.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   parseUrl/1
   , dbOpts/1
   , agencyOpts/1
   , warnMsg/3
   , getListValue/3
   , randElement/1
   , toBinary/1
   , agMethod/1
   , syncSend/2
]).

-spec parseUrl(binary()) -> dbOpts() | {error, invalidUrl}.
parseUrl(<<"http://", Rest/binary>>) ->
   parseUrl(tcp, Rest);
parseUrl(<<"https://", Rest/binary>>) ->
   parseUrl(ssl, Rest);
parseUrl(_) ->
   {error, invalidUrl}.

-spec parseUrl(protocol(), binary()) -> dbOpts().
parseUrl(Protocol, Rest) ->
   {Host, _Path} =
      case binary:split(Rest, <<"/">>, [trim]) of
         [UrlHost] ->
            {UrlHost, <<"/">>};
         [UrlHost, UrlPath] ->
            {UrlHost, <<"/", UrlPath/binary>>}
      end,

   {Hostname, Port} =
      case binary:split(Host, <<":">>, [trim]) of
         [Host] ->
            case Protocol of
               tcp ->
                  {Host, 80};
               ssl ->
                  {Host, 443}
            end;
         [UrlHostname, UrlPort] ->
            {UrlHostname, binary_to_integer(UrlPort)}
      end,
   #dbOpts{host = Host, port = Port, hostname = binary_to_list(Hostname), protocol = Protocol}.

-spec dbOpts(list()) -> dbOpts().
dbOpts(DbCfgs) ->
   BaseUrl = ?AgGetListKV(baseUrl, DbCfgs, ?AgDefBaseUrl),
   DbName = ?AgGetListKV(dbName, DbCfgs, ?AgDefDbName),
   User = ?AgGetListKV(user, DbCfgs, ?AgDefUser),
   Password = ?AgGetListKV(password, DbCfgs, ?AgDefPassWord),
   PoolSize = ?AgGetListKV(poolSize, DbCfgs, ?AgDefPoolSize),
   VstSize = ?AgGetListKV(vstSize, DbCfgs, ?AgDefVstSize),
   DbOpts = agMiscUtils:parseUrl(BaseUrl),
   DbOpts#dbOpts{dbName = DbName, user = User, password = Password, poolSize = PoolSize, vstSize = VstSize}.

-spec agencyOpts(list()) -> agencyOpts().
agencyOpts(AgencyCfgs) ->
   IsReconnect = ?AgGetListKV(reconnect, AgencyCfgs, ?AgDefIsReConn),
   AgencySlg = ?AgGetListKV(agencySlg, AgencyCfgs, ?AgDefAgencySlg),
   BacklogSize = ?AgGetListKV(backlogSize, AgencyCfgs, ?AgDefBacklogSize),
   Min = ?AgGetListKV(reConnTimeMin, AgencyCfgs, ?AgDefReConnMin),
   Max = ?AgGetListKV(reConnTimeMax, AgencyCfgs, ?AgDefReConnMax),
   #agencyOpts{reconnect = IsReconnect, agencySlg = AgencySlg, backlogSize = BacklogSize, reConnTimeMin = Min, reConnTimeMax = Max}.

-spec getListValue(term(), list(), term()) -> term().
getListValue(Key, List, Default) ->
   case lists:keyfind(Key, 1, List) of
      false ->
         Default;
      {_Key, Value} ->
         Value
   end.

-spec warnMsg(term(), string(), [term()]) -> ok.
warnMsg(Tag, Format, Data) ->
   error_logger:warning_msg("[~p] " ++ Format, [Tag | Data]).

-spec randElement([term()]) -> term().
randElement([X]) ->
   X;
randElement([_ | _] = List) ->
   T = list_to_tuple(List),
   element(rand:uniform(tuple_size(T)), T).

-spec toBinary(term()) -> binary().
toBinary(Value) when is_integer(Value) -> integer_to_binary(Value);
toBinary(Value) when is_list(Value) -> list_to_binary(Value);
toBinary(Value) when is_float(Value) -> float_to_binary(Value, [{decimals, 6}, compact]);
toBinary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
toBinary(Value) when is_binary(Value) -> Value;
toBinary(Value) -> term_to_binary(Value).

agMethod(0) -> <<"Delete">>;
agMethod(1) -> <<"Get">>;
agMethod(2) -> <<"Post">>;
agMethod(3) -> <<"Put">>;
agMethod(4) -> <<"Head">>;
agMethod(5) -> <<"Patch">>;
agMethod(6) -> <<"Options">>.

%% This is a generic "port_command" interface used by TCP, UDP, SCTP, depending
%% on the driver it is mapped to, and the "Data". It actually sends out data,--
%% NOT delegating this task to any back-end.  For SCTP, this function MUST NOT
%% be called directly -- use "sendmsg" instead:
%%
syncSend(S, Data) ->
   MRef = monitor(port, S),
   MRefBin = term_to_binary(MRef, [local]),
   MRefBinSize = byte_size(MRefBin),
   MRefBinSize = MRefBinSize band 16#FFFF,
   try
      erlang:port_command(S, [<<MRefBinSize:16, MRefBin/binary>>, Data], [])
   of
      false -> % Port busy when nosuspend option was passed
         {error, busy};
      true ->
         receive
            {inet_reply, S, Status, MRef} ->
               demonitor(MRef, [flush]),
               Status;
            {'DOWN', MRef, _, _, _Reason} ->
               {error, closed}
         end
   catch error: _ ->
      {error, einval}
   end.

