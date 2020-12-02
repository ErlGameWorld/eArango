-module(agAgencyUtils).
-include("agVstCli.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   cancelTimer/1
   , dealClose/3
   , reConnTimer/2
   , agencyReply/2
   , agencyReply/4
   , initReConnState/3
   , resetReConnState/1
   , updateReConnState/1
]).

-spec dealClose(srvState(), cliState(), term()) -> {ok, srvState(), cliState()}.
dealClose(SrvState, #cliState{requestsIns = RequestsIns, requestsOuts = RequestsOuts, curInfo = CurInfo} = ClientState, Reply) ->
   agencyReply(CurInfo, Reply),
   agencyReplyAll(RequestsOuts, RequestsIns, Reply),
   reConnTimer(SrvState, ClientState#cliState{requestsIns = [], requestsOuts = [], backlogNum = 0, revStatus = leisure, curInfo = undefined, recvState = undefined}).

-spec reConnTimer(srvState(), cliState()) -> {ok, srvState(), cliState()}.
reConnTimer(#srvState{reconnectState = undefined} = SrvState, CliState) ->
   {ok, {SrvState#srvState{socket = undefined}, CliState}};
reConnTimer(#srvState{reconnectState = ReconnectState} = SrvState, CliState) ->
   #reConnState{current = Current} = MewReconnectState = agAgencyUtils:updateReConnState(ReconnectState),
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

-spec initReConnState(boolean(), pos_integer(), pos_integer()) -> reconnectState() | undefined.
initReConnState(IsReconnect, Min, Max) ->
   case IsReconnect of
      true ->
         #reConnState{min = Min, max = Max, current = Min};
      false ->
         undefined
   end.

-spec resetReConnState(undefined | reconnectState()) -> reconnectState() | undefined.
resetReConnState(#reConnState{min = Min} = ReconnectState) ->
   ReconnectState#reConnState{current = Min}.

-spec updateReConnState(reconnectState()) -> reconnectState().
updateReConnState(#reConnState{current = Current, max = Max} = ReconnectState) ->
   NewCurrent = Current + Current,
   ReconnectState#reConnState{current = minCur(NewCurrent, Max)}.

minCur(A, B) when B >= A ->
   A;
minCur(_, B) ->
   B.

