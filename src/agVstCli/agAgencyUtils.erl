-module(agAgencyUtils).
-include("agVstCli.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   cancelTimer/1
   , dealClose/3
   , reConnTimer/2
   , agencyReply/4
   , agencyReTimeout/3
   , initReConnState/3
   , resetReConnState/1
   , updateReConnState/1
]).

-spec dealClose(srvState(), cliState(), term()) -> {ok, srvState(), cliState()}.
dealClose(SrvState, ClientState, Reply) ->
   agencyReplyAll(Reply),
   reConnTimer(SrvState, ClientState#cliState{backlogNum = 0, revStatus = ?AgUndef}).

-spec reConnTimer(srvState(), cliState()) -> {ok, srvState(), cliState()}.
reConnTimer(#srvState{reConnState = undefined} = SrvState, CliState) ->
   {ok, {SrvState#srvState{socket = undefined}, CliState}};
reConnTimer(#srvState{reConnState = ReConnState} = SrvState, CliState) ->
   #reConnState{current = Current} = MewReConnState = agAgencyUtils:updateReConnState(ReConnState),
   TimerRef = erlang:send_after(Current, self(), ?AgMDoDBConn),
   {ok, SrvState#srvState{reConnState = MewReConnState, socket = undefined, timerRef = TimerRef}, CliState}.

-spec agencyReply(undefined | pid(), messageId(), undefined | reference(), term()) -> ok.
agencyReply(undefined, MessageId, TimerRef, _Reply) ->
   erlang:erase(MessageId),
   agAgencyUtils:cancelTimer(TimerRef);
agencyReply(timeOut, MessageId, TimerRef, _Reply) ->
   erlang:erase(MessageId),
   agAgencyUtils:cancelTimer(TimerRef);
agencyReply(FormPid, MessageId, TimerRef, Reply) ->
   erlang:erase(MessageId),
   agAgencyUtils:cancelTimer(TimerRef),
   catch FormPid ! #agReqRet{messageId = MessageId, reply = Reply},
   ok.

-spec agencyReTimeout(undefined | pid(), messageId(), term()) -> ok.
agencyReTimeout(undefined, _MessageId, _Reply) ->
   ok;
agencyReTimeout(timeOut, _MessageId, _Reply) ->
   ok;
agencyReTimeout(FormPid, MessageId, Reply) ->
   catch FormPid ! #agReqRet{messageId = MessageId, reply = Reply},
   ok.

-spec agencyReplyAll(term()) -> ok.
agencyReplyAll(Reply) ->
   [agencyReply(PidForm, MessageId, TimeRef, Reply) || {MessageId, {PidForm, TimeRef, _, _}} <- erlang:get()],
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

-spec initReConnState(boolean(), pos_integer(), pos_integer()) -> reConnState() | undefined.
initReConnState(IsReconnect, Min, Max) ->
   case IsReconnect of
      true ->
         #reConnState{min = Min, max = Max, current = Min};
      false ->
         undefined
   end.

-spec resetReConnState(undefined | reConnState()) -> reConnState() | undefined.
resetReConnState(#reConnState{min = Min} = ReConnState) ->
   ReConnState#reConnState{current = Min}.

-spec updateReConnState(reConnState()) -> reConnState().
updateReConnState(#reConnState{current = Current, max = Max} = ReConnState) ->
   NewCurrent = Current + Current,
   ReConnState#reConnState{current = if NewCurrent >= Max -> Max; true -> NewCurrent end}.

