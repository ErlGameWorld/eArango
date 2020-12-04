%% agency 管理进程的名称
-define(agAgencyPoolMgr, agAgencyPoolMgr).

%% beam cache 模块名
-define(agBeamPool, agBeamPool).
-define(agBeamAgency, agBeamAgency).

-define(AgUndef, 0).                %% Wait One Chunk start
-define(AgCHeader, 1).              %% Wait One Chunk header
-define(AgCBody, 2).                %% Wait One Chunk Body
-define(AgCBodyStart, 3).           %% Ret Start Wait One Chunk Body
-define(AgCBodyGoOn, 4).            %% Ret Go On Wait One Chunk Body
-define(AgCDone, 5).                %% receve one Chunk done
-define(AgMDone, 6).                %% receve one message done

%% pidFrom pid() to reply; undefiend  discard; timeOut 该请求标记为过期了 discard
-record(msgIdCache, {pidFrom, timerRef, chunkCnt, msgBuffer}).

-define(AgMBIdx, 4).
-define(AgCCIdx, 3).
-define(AgTRIdx, 2).
-define(AgPFIdx, 1).

-define(AgHeaderSize, 24).

%% 默认选项定义
-define(AgDefBaseUrl, <<"http://192.168.0.88:8529">>).
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
-define(AgDefVstSize, 3145728).
-define(AgDefAgencySlg, poll).          %% bind rand  poll
-define(AgDefPid, self()).
-define(AgDefSocketOpts, [binary, {active, true}, {nodelay, true}, {delay_send, true}, {keepalive, true}, {recbuf, 1048576}, {send_timeout, 5000}, {send_timeout_close, true}]).

-define(AgGetListKV(Key, List), agMiscUtils:getListValue(Key, List, undefined)).
-define(AgGetListKV(Key, List, Default), agMiscUtils:getListValue(Key, List, Default)).
-define(AgWarn(Tag, Format, Data), agMiscUtils:warnMsg(Tag, Format, Data)).

-define(AgMDoDBConn, mDoDBConn).
-define(AgUpgradeInfo, <<"VST/1.1\r\n\r\n">>).

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
   dbName :: binary(),
   reConnState :: undefined | reConnState(),
   socket :: undefined | ssl:sslsocket(),
   timerRef :: undefined | reference()
}).

-record(cliState, {
   backlogSize = 0 :: integer(),
   revStatus = ?AgUndef :: pos_integer(),
   backlogNum = 0 :: integer(),
   messageId = 0 :: pos_integer(),
   chunkIdx = 0 :: pos_integer(),
   chunkSize = 0 :: pos_integer(),
   chunkBuffer = <<>> :: binary()
}).

-record(recvState, {
   revStatus = ?AgUndef :: pos_integer(),
   messageId = 0 :: pos_integer(),
   chunkCnt = -1 :: integer(),
   msgBuffer = <<>> :: binary(),
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
   user :: binary(),
   password :: binary(),
   socketOpts :: socketOpts()
}).

-record(agencyOpts, {
   reconnect :: boolean(),
   vstSize :: pos_integer(),
   agencySlg :: agencySlg(),
   backlogSize :: backlogSize(),
   reConnTimeMin :: pos_integer(),
   reConnTimeMax :: pos_integer()
}).

-type miRequest() :: #agReq{}.
-type miRequestRet() :: #agReqRet{}.
-type srvState() :: #srvState{}.
-type cliState() :: #cliState{}.
-type reConnState() :: #reConnState{}.
-type recvState() :: #recvState{}.

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
-type agencySlg() :: bind | rand | poll.
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
{vstSize, pos_integer()} |
{agencySlg, agencySlg()} |
{backlogSize, backlogSize()} |
{reConnTimeMin, pos_integer()} |
{reConnTimeMax, pos_integer()}.

-type dbCfgs() :: [dbCfg()].
-type dbOpts() :: #dbOpts{}.
-type agencyCfgs() :: [agencyCfg()].
-type agencyOpts() :: #agencyOpts{}.
