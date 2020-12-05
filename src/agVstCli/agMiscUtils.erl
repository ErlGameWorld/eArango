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
   , randomElement/1
   , toBinary/1
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
      {Key, Value} ->
         Value
   end.

-spec warnMsg(term(), string(), [term()]) -> ok.
warnMsg(Tag, Format, Data) ->
   error_logger:warning_msg("[~p] " ++ Format, [Tag | Data]).

-spec randomElement([term()]) -> term().
randomElement([X]) ->
   X;
randomElement([_ | _] = List) ->
   T = list_to_tuple(List),
   element(rand:uniform(tuple_size(T)), T).

-spec toBinary(term()) -> binary().
toBinary(Value) when is_integer(Value) -> integer_to_binary(Value);
toBinary(Value) when is_list(Value) -> list_to_binary(Value);
toBinary(Value) when is_float(Value) -> float_to_binary(Value, [{decimals, 6}, compact]);
toBinary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
toBinary(Value) when is_binary(Value) -> Value;
toBinary([Tuple | PropList] = Value) when is_list(PropList) and is_tuple(Tuple) ->
   lists:map(fun({K, V}) -> {toBinary(K), toBinary(V)} end, Value);
toBinary(Value) -> term_to_binary(Value).

