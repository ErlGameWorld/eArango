-module(agAgencyUtils).
-include("agVstCli.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   cancelTimer/1
   , dealClose/3
   , reconnectTimer/2
   , agencyReply/2
   , agencyReply/4
   , initReconnectState/3
   , resetReconnectState/1
   , updateReconnectState/1
]).

-spec dealClose(srvState(), cliState(), term()) -> {ok, srvState(), cliState()}.
dealClose(SrvState, #cliState{requestsIns = RequestsIns, requestsOuts = RequestsOuts, curInfo = CurInfo} = ClientState, Reply) ->
   agencyReply(CurInfo, Reply),
   agencyReplyAll(RequestsOuts, RequestsIns, Reply),
   reconnectTimer(SrvState, ClientState#cliState{requestsIns = [], requestsOuts = [], backlogNum = 0, revStatus = leisure, curInfo = undefined, recvState = undefined}).

-spec reconnectTimer(srvState(), cliState()) -> {ok, srvState(), cliState()}.
reconnectTimer(#srvState{reconnectState = undefined} = SrvState, CliState) ->
   {ok, {SrvState#srvState{socket = undefined}, CliState}};
reconnectTimer(#srvState{reconnectState = ReconnectState} = SrvState, CliState) ->
   #reConnState{current = Current} = MewReconnectState = agAgencyUtils:updateReconnectState(ReconnectState),
   TimerRef = erlang:send_after(Current, self(), ?AgMDoNetConn),
   {ok, SrvState#srvState{reconnectState = MewReconnectState, socket = undefined, timerRef = TimerRef}, CliState}.

-spec agencyReply(term(), term()) -> ok.
agencyReply({undefined, _RequestId, TimerRef}, _Reply) ->
   agAgencyUtils:cancelTimer(TimerRef);
agencyReply({PidForm, RequestId, TimerRef}, Reply) ->
   agAgencyUtils:cancelTimer(TimerRef),
   catch PidForm ! #agReqRet{messageId = RequestId, reply = Reply},
   ok;
agencyReply(undefined, _RequestRet) ->
   ok.

-spec agencyReply(undefined | pid(), messageId(), undefined | reference(), term()) -> ok.
agencyReply(undefined, MessageId, TimerRef, _Reply) ->
   agAgencyUtils:cancelTimer(TimerRef);
agencyReply(FormPid, RequestId, TimerRef, Reply) ->
   agAgencyUtils:cancelTimer(TimerRef),
   catch FormPid ! #agReqRet{messageId = RequestId, reply = Reply},
   ok.

-spec agencyReplyAll(list(), list(), term()) -> ok.
agencyReplyAll(RequestsOuts, RequestsIns, Reply) ->
   [agencyReply(FormPid, RequestId, undefined, Reply) || #agReq{messageId = RequestId, fromPid = FormPid} <- RequestsOuts],
   [agencyReply(FormPid, RequestId, undefined, Reply) || #agReq{messageId = RequestId, fromPid = FormPid} <- lists:reverse(RequestsIns)],
   ok.

-spec cancelTimer(undefined | reference()) -> ok.
cancelTimer(undefined) -> ok;
cancelTimer(TimerRef) ->
   case erlang:cancel_timer(TimerRef) of
      false ->
         receive
            {timeout, TimerRef, _Msg} ->
               %% discard the timeout msg
               ok
         after 0 ->
            ok
         end;
      _ ->
         %% Timer already run
         ok
   end.

-spec initReconnectState(boolean(), pos_integer(), pos_integer()) -> reconnectState() | undefined.
initReconnectState(IsReconnect, Min, Max) ->
   case IsReconnect of
      true ->
         #reConnState{min = Min, max = Max, current = Min};
      false ->
         undefined
   end.

-spec resetReconnectState(undefined | reconnectState()) -> reconnectState() | undefined.
resetReconnectState(#reConnState{min = Min} = ReconnectState) ->
   ReconnectState#reConnState{current = Min}.

-spec updateReconnectState(reconnectState()) -> reconnectState().
updateReconnectState(#reConnState{current = Current, max = Max} = ReconnectState) ->
   NewCurrent = Current + Current,
   ReconnectState#reConnState{current = minCur(NewCurrent, Max)}.

minCur(A, B) when B >= A ->
   A;
minCur(_, B) ->
   B.

