%% agency 管理进程的名称
-define(agAgencyPoolMgr, agAgencyPoolMgr).

%% beam cache 模块名
-define(agBeamPool, agBeamPool).
-define(agBeamAgency, agBeamAgency).

-define(AgCUndef, 0).               %% Wait One Chunk start
-define(AgCHeader, 1).              %% Wait One Chunk header
-define(AgCBody, 2).                %% Wait One Chunk Body
-define(AgCDone, 3).                %% Wait One Chunk Receive Over
-define(AgMDone, 4).                %% Wait One Message Over
-define(AgCBodyStart, 5).           %% Ret Start Wait One Chunk Body
-define(AgCBodyGoOn, 6).            %% Ret Go On Wait One Chunk Body

%% IMY-todo  考虑多个消息回复的的时候 如果有消息 此时进程自动可能不存在 需要重新订阅获取
%% pidFrom pid() to reply; undefiend  discard; waitSend 起送定时器等待requester来获取 过期就删除
-record(msgIdCache, {pidFrom, timerRef, chunkCnt, msgBuffer}).

-define(AgMBIdx, 4).
-define(AgCCIdx, 3).

%% 默认选项定义
-define(AgDefBaseUrl, <<"http://127.0.0.1:8529">>).
-define(AgDefDbName, <<"_system">>).
-define(AgDefUser, <<"root">>).
-define(AgDefPassWord, <<"156736">>).
-define(AgDefBacklogSize, 1024).
-define(AgDefConnTimeout, 5000).
-define(AgDefPoolSize, 16).
-define(AgDefIsReConn, true).
-define(AgDefReConnMin, 500).
-define(AgDefReConnMax, 120000).
-define(AgDefTimeout, infinity).
-define(AgDefPid, self()).
-define(AgDefSocketOpts, [binary, {active, true}, {nodelay, true}, {delay_send, true}, {keepalive, true}, {recbuf, 1048576}, {send_timeout, 5000}, {send_timeout_close, true}]).

-define(AgGetListKV(Key, List), agMiscUtils:getListValue(Key, List, undefined)).
-define(AgGetListKV(Key, List, Default), agMiscUtils:getListValue(Key, List, Default)).
-define(AgWarn(Tag, Format, Data), agMiscUtils:warnMsg(Tag, Format, Data)).

-define(AgMDoNetConn, mDoNetConn).

-record(agReq, {
   method :: method()
   , path :: path()
   , queryPars :: queryPars()
   , headers :: headers()
   , body :: body()
   , messageId :: pos_integer()
   , fromPid :: pid()
   , overTime = infinity :: timeout()
   , isSystem = false :: boolean()
}).

-record(agReqRet, {
   messageId :: messageId(),
   reply :: term()
}).

-record(reConnState, {
   min :: non_neg_integer(),
   max :: non_neg_integer() | infinity,
   current :: non_neg_integer() | undefined
}).

-record(srvState, {
   poolName :: poolName(),
   serverName :: serverName(),
   userPassWord :: binary(),
   host :: binary(),
   dbName :: binary(),
   reconnectState :: undefined | reconnectState(),
   socket :: undefined | ssl:sslsocket(),
   timerRef :: undefined | reference()
}).

-record(cliState, {
   revStatus = ?AgCUndef :: pos_integer(),
   backlogNum = 0 :: integer(),
   backlogSize = 0 :: integer(),
   messageId = 0 :: pos_integer(),
   chunkIdx = 0 :: pos_integer(),
   chunkSize = 0 :: pos_integer(),
   chunkBuffer = <<>> :: binary()
}).

-record(dbOpts, {
   host :: host(),
   port :: 0..65535,
   hostname :: hostName(),
   dbName :: binary(),
   protocol :: protocol(),
   poolSize :: poolSize(),
   userPassword :: binary(),
   socketOpts :: socketOpts()
}).

-record(agencyOpts, {
   reconnect :: boolean(),
   backlogSize :: backlogSize(),
   reconnectTimeMin :: pos_integer(),
   reconnectTimeMax :: pos_integer()
}).

-type miRequest() :: #agReq{}.
-type miRequestRet() :: #agReqRet{}.
-type srvState() :: #srvState{}.
-type cliState() :: #cliState{}.
-type reconnectState() :: #reConnState{}.

-type poolName() :: atom().
-type poolNameOrSocket() :: atom() | socket().
-type serverName() :: atom().
-type protocol() :: ssl | tcp.
-type method() :: binary().
-type queryPars() :: map().
-type headers() :: map().
-type body() :: iodata() | undefined.
-type path() :: binary().
-type host() :: binary().
-type hostName() :: string().
-type poolSize() :: pos_integer().
-type backlogSize() :: pos_integer() | infinity.
-type messageId() :: pos_integer().
-type socket() :: inet:socket() | ssl:sslsocket().
-type socketOpts() :: [gen_tcp:connect_option() | ssl:tls_client_option()].
-type error() :: {error, term()}.

-type dbCfg() ::
{baseUrl, binary()} |
{dbName, binary()} |
{user, binary()} |
{password, binary()} |
{poolSize, poolSize()} |
{socketOpts, socketOpts()}.

-type agencyCfg() ::
{reconnect, boolean()} |
{backlogSize, backlogSize()} |
{reconnectTimeMin, pos_integer()} |
{reconnectTimeMax, pos_integer()}.

-type dbCfgs() :: [dbCfg()].
-type dbOpts() :: #dbOpts{}.
-type agencyCfgs() :: [agencyCfg()].
-type agencyOpts() :: #agencyOpts{}.

%% http header 头
%% -type header() ::
%%    'Cache-Control' |
%%    'Connection' |
%%    'Date' |
%%    'Pragma'|
%%    'Transfer-Encoding' |
%%    'Upgrade' |
%%    'Via' |
%%    'Accept' |
%%    'Accept-Charset'|
%%    'Accept-Encoding' |
%%    'Accept-Language' |
%%    'Authorization' |
%%    'From' |
%%    'Host' |
%%    'If-Modified-Since' |
%%    'If-Match' |
%%    'If-None-Match' |
%%    'If-Range'|
%%    'If-Unmodified-Since' |
%%    'Max-Forwards' |
%%    'Proxy-Authorization' |
%%    'Range'|
%%    'Referer' |
%%    'User-Agent' |
%%    'Age' |
%%    'Location' |
%%    'Proxy-Authenticate'|
%%    'Public' |
%%    'Retry-After' |
%%    'Server' |
%%    'Vary' |
%%    'Warning'|
%%    'Www-Authenticate' |
%%    'Allow' |
%%    'Content-Base' |
%%    'Content-Encoding'|
%%    'Content-Language' |
%%    'Content-Length' |
%%    'Content-Location'|
%%    'Content-Md5' |
%%    'Content-Range' |
%%    'Content-Type' |
%%    'Etag'|
%%    'Expires' |
%%    'Last-Modified' |
%%    'Accept-Ranges' |
%%    'Set-Cookie'|
%%    'Set-Cookie2' |
%%    'X-Forwarded-For' |
%%    'Cookie' |
%%    'Keep-Alive' |
%%    'Proxy-Connection' |
%%    binary() |
%%    string().

