-module(vst).

-export([
   authorize/1,
   connect/2,
   request/2,
   vst_maxsize/0
]).

assign_map(_acc@1, _key@1, _value@1) ->
   case _acc@1 of
      #{_key@1 := _} -> _acc@1;
      #{} -> _acc@1#{_key@1 => _value@1};
      _ -> #{_key@1 => _value@1}
   end.

assign_split([<<>>, _rest@1], _value@1, _acc@1, _pattern@1) ->
   _parts@1 = binary:split(_rest@1, _pattern@1),
   case _acc@1 of
      [_ | _] ->
         [assign_split(_parts@1, _value@1, none, _pattern@1) | _acc@1];
      none ->
         [assign_split(_parts@1, _value@1, none, _pattern@1)];
      _ -> _acc@1
   end;
assign_split([_key@1, _rest@1], _value@1, _acc@1, _pattern@1) ->
   _parts@1 = binary:split(_rest@1, _pattern@1),
   case _acc@1 of
      #{_key@1 := _current@1} ->
         _acc@1#{_key@1 =>
         assign_split(_parts@1, _value@1, _current@1, _pattern@1)};
      #{} ->
         _acc@1#{_key@1 => assign_split(_parts@1, _value@1, none, _pattern@1)};
      _ ->
         #{_key@1 => assign_split(_parts@1, _value@1, none, _pattern@1)}
   end;
assign_split([<<>>], nil, _acc@1, __pattern@1) ->
   case _acc@1 of
      [_ | _] -> _acc@1;
      _ -> []
   end;
assign_split([<<>>], _value@1, _acc@1, __pattern@1) ->
   case _acc@1 of
      [_ | _] -> [_value@1 | _acc@1];
      none -> [_value@1];
      _ -> _acc@1
   end;
assign_split([_key@1], _value@1, _acc@1, __pattern@1) ->
   assign_map(_acc@1, _key@1, _value@1).

authorize(#{socket := _socket@1, username := _un@1, password := _pw@1} = _state@1) ->
   case eVPack:encode([1, 1000, <<"plain">>, _un@1, _pw@1]) of
      {ok, _auth@1} ->
         case send_stream(_socket@1, build_stream(_auth@1)) of
            ok ->
               case recv_header(_socket@1) of
                  {ok, _header@1} ->
                     case recv_stream(_socket@1, _header@1) of
                        {ok, _stream@1} ->
                           case decode_stream(_stream@1) of
                              {ok, [[1, 2, 200, __headers@2] | __body@1]} ->
                                 ok;
                              _@1 ->
                                 case _@1 of
                                    {ok, [[1, 2, _status@1, __headers@1], _body@1 | _]} ->
                                       {error, #{
                                          '__exception__' => true,
                                          error_num => nil,
                                          status => _status@1,
                                          message => proplists:get_value(<<"errorMessage">>, _body@1),
                                          endpoint =>
                                          case _state@1 of
                                             #{endpoint := _@3} ->
                                                _@3;
                                             _@3 when erlang:is_map(_@3) ->
                                                erlang:error({badkey, endpoint, _@3});
                                             _@3 ->
                                                _@3:endpoint()
                                          end}};
                                    {error, _reason@1} ->
                                       {error, _reason@1};
                                    _@2 ->
                                       erlang:error({with_clause, _@2})
                                 end
                           end;
                        _@1 ->
                           case _@1 of
                              {ok,
                                 [[1, 2, _status@1, __headers@1], _body@1 | _]} ->
                                 {error, #{
                                    '__exception__' => true,
                                    error_num => nil,
                                    status => _status@1,
                                    message => proplists:get_value(<<"errorMessage">>, _body@1),
                                    endpoint =>
                                    case _state@1 of
                                       #{endpoint := _@3} ->
                                          _@3;
                                       _@3 when erlang:is_map(_@3) ->
                                          erlang:error({badkey, endpoint, _@3});
                                       _@3 -> _@3:endpoint()
                                    end}};
                              {error, _reason@1} ->
                                 {error, _reason@1};
                              _@2 -> erlang:error({with_clause, _@2})
                           end
                     end;
                  _@1 ->
                     case _@1 of
                        {ok,
                           [[1, 2, _status@1, __headers@1], _body@1
                              | _]} ->
                           {error, #{
                              '__exception__' => true,
                              error_num => nil, status => _status@1,
                              message => proplists:get_value(<<"errorMessage">>, _body@1),
                              endpoint =>
                              case _state@1 of
                                 #{endpoint := _@3} -> _@3;
                                 _@3 when erlang:is_map(_@3) ->
                                    erlang:error({badkey,
                                       endpoint,
                                       _@3});
                                 _@3 -> _@3:endpoint()
                              end}};
                        {error, _reason@1} -> {error, _reason@1};
                        _@2 -> erlang:error({with_clause, _@2})
                     end
               end;
            _@1 ->
               case _@1 of
                  {ok, [[1, 2, _status@1, __headers@1], _body@1 | _]} ->
                     {error,
                        #{
                           '__exception__' => true, error_num => nil,
                           status => _status@1,
                           message => proplists:get_value(<<"errorMessage">>, _body@1),
                           endpoint =>
                           case _state@1 of
                              #{endpoint := _@3} -> _@3;
                              _@3 when erlang:is_map(_@3) ->
                                 erlang:error({badkey,
                                    endpoint,
                                    _@3});
                              _@3 -> _@3:endpoint()
                           end}};
                  {error, _reason@1} -> {error, _reason@1};
                  _@2 -> erlang:error({with_clause, _@2})
               end
         end;
      _@1 ->
         case _@1 of
            {ok, [[1, 2, _status@1, __headers@1], _body@1 | _]} ->
               {error, #{
                  '__exception__' => true,
                  error_num => nil,
                  status => _status@1,
                  message => proplists:get_value(<<"errorMessage">>, _body@1),
                  endpoint =>
                  case _state@1 of
                     #{endpoint := _@3} -> _@3;
                     _@3 when erlang:is_map(_@3) ->
                        erlang:error({badkey, endpoint, _@3});
                     _@3 -> _@3:endpoint()
                  end}};
            {error, _reason@1} -> {error, _reason@1};
            _@2 -> erlang:error({with_clause, _@2})
         end
   end.

body_for(<<>>) -> {ok, <<>>};
body_for(_body@1) ->
   eVPack:encode(_body@1).

body_from([]) -> nil;
body_from([_body@1]) -> _body@1;
body_from(_body@1) -> _body@1.

build_stream(_message@1) ->
   case chunk_every(_message@1, 30696) of
      [_first_chunk@1 | _rest_chunks@1] ->
         _n_chunks@1 = erlang:length([_first_chunk@1
            | _rest_chunks@1]),
         _msg_length@1 = erlang:byte_size(_message@1) +
            _n_chunks@1 * 24,
         _rest_chunks@2 =
            lists:reverse(lists:flodl(
               fun(_n@1, _@1) ->
                  case _rest_chunks@1 /= [] of
                     true ->
                        [prepend_chunk(lists:nth(_n@1, _rest_chunks@1), _n@1, 0, 0, _msg_length@1) | _@1];
                     false -> _@1
                  end
               end, [], lists:seq(1, erlang:length(_rest_chunks@1)))),
         [prepend_chunk(_first_chunk@1, _n_chunks@1, 1, 0, _msg_length@1) | _rest_chunks@2];
      _only_chunk@1 ->
         prepend_chunk(_only_chunk@1, 1, 1, 0, erlang:byte_size(_message@1) + 24)
   end.

chunk_every(_bytes@1, _size@1) when erlang:byte_size(_bytes@1) =< _size@1 ->
   _bytes@1;
chunk_every(_bytes@1, _size@1) ->
   <<_chunk@1:_size@1/binary, _rest@1/binary>> = _bytes@1,
   [_chunk@1 | (chunk_every(_rest@1, _size@1))].

connect(#{addr := _addr@1, 'ssl?' := _ssl}, _opts@1) ->
   _mod@1 = case _ssl of
               _@1 when _@1 =:= false orelse _@1 =:= nil -> gen_tcp;
               _ -> ssl
            end,
   _transport_opts@1 = case _ssl of
                          _@2 when _@2 =:= false orelse _@2 =:= nil ->
                             tcp_opts;
                          _ -> ssl_opts
                       end,
   _transport_opts@2 = proplists:get_value(_transport_opts@1, _opts@1, []),
   _connect_timeout@1 = proplists:get(connect_timeout, _opts@1, 5000),
   _options@1 = lists:merge(_transport_opts@2, [{packet, raw}, {mode, binary}, {active, false}]),
   case _mod@1:connect(addr_for(_addr@1), port_for(_addr@1), _options@1, _connect_timeout@1) of
      {ok, _port@1} ->
         case _mod@1:send(_port@1, <<"VST/1.1\r\n\r\n">>) of
            ok -> {ok, {_mod@1, _port@1}};
            _@3 -> _@3
         end;
      _@3 -> _@3
   end.

decode_pair({_key@1, _value@1}, _acc@1) ->
   case case _key@1 /= <<>> of
           false -> false;
           true -> binary:last(_key@1) == 93
        end
   of
      false -> assign_map(_acc@1, _key@1, _value@1);
      true ->
         _subkey@1 = binary:part(_key@1,
            0,
            erlang:byte_size(_key@1) - 1),
         assign_split(binary:split(_subkey@1, <<"[">>),
            _value@1,
            _acc@1,
            binary:compile_pattern(<<"][">>))
   end.

decode_stream(_@1) -> decode_stream(_@1, []).

decode_stream(<<>>, _acc@1) -> {ok, _acc@1};
decode_stream(_stream@1, _acc@1) ->
   case eVPack:decode(_stream@1) of
      {ok, {_term@1, _rest@1}} ->
         decode_stream(_rest@1, erlang:'++'(_acc@1, [_term@1]));
      {ok, _term@2} -> {ok, erlang:'++'(_acc@1, [_term@2])};
      {error, _reason@1} -> {error, _reason@1}
   end.

headers_for(#{} = _headers@1) -> _headers@1;
headers_for(_headers@1)
   when erlang:is_list(_headers@1) ->
   maps:from_list(_headers@1).

method_for(delete) -> 0;
method_for(get) -> 1;
method_for(post) -> 2;
method_for(put) -> 3;
method_for(head) -> 4;
method_for(patch) -> 5;
method_for(options) -> 6;
method_for(_) -> -1.

port_for({unix, __path@1}) -> 0;
port_for({tcp, __host@1, _port@1}) -> _port@1.

prepend_chunk(_chunk@1, _chunk_n@1, _is_first@1,
   _msg_id@1, _msg_length@1) ->
   <<(24 + erlang:byte_size(_chunk@1)):32/integer-little,
      (binary:decode_unsigned(<<_chunk_n@1:31/integer,
         _is_first@1:1/integer>>,
         little)):32/integer,
      _msg_id@1:64/integer-little,
      _msg_length@1:64/integer-little, _chunk@1/binary>>.

query_for(nil) -> #{}.

recv_chunk({_mod@1, _port@1}, _chunk_length@1) ->
   _mod@1:recv(_port@1, _chunk_length@1 - 24).

recv_header({_mod@1, _port@1}) ->
   case _mod@1:recv(_port@1, 24) of
      {ok,
         <<_chunk_length@1:32/integer-little,
            _chunk_x@1:32/integer, _msg_id@1:64/integer-little,
            _msg_length@1:64/integer-little>>} ->
         <<_chunk_n@1:31/integer, _is_first@1:1/integer>> =
            <<_chunk_x@1:32/integer-little>>,
         {ok,
            [_chunk_length@1,
               _chunk_n@1,
               _is_first@1,
               _msg_id@1,
               _msg_length@1]};
      {error, _reason@1} -> {error, _reason@1}
   end.

recv_stream(_socket@1,
   [_chunk_length@1, 1, 1, __msg_id@1, __msg_length@1]) ->
   recv_chunk(_socket@1, _chunk_length@1);
recv_stream(_socket@1,
   [_chunk_length@1,
      _n_chunks@1,
      1,
      __msg_id@1,
      __msg_length@1]) ->
   case recv_chunk(_socket@1, _chunk_length@1) of
      {ok, _buffer@1} ->
         case recv_stream(_socket@1, _n_chunks@1, _buffer@1) of
            {ok, _stream@1} -> {ok, _stream@1};
            _@1 -> _@1
         end;
      _@1 -> _@1
   end.

recv_stream(_socket@1, _n_chunks@1, _buffer@1) ->
   lists:reduce_while(lists:seq(1, _n_chunks@1 - 1),
      _buffer@1,
      fun(_n@1, _buffer@2) ->
         case recv_header(_socket@1) of
            {ok, [_chunk_length@1, _, _, _, _]} ->
               case recv_chunk(_socket@1, _chunk_length@1) of
                  {ok, _chunk@1} ->
                     case _n@1 == _n_chunks@1 - 1 of
                        false ->
                           {cont, <<_buffer@2/binary, _chunk@1/binary>>};
                        true ->
                           {halt, {ok, <<_buffer@2/binary, _chunk@1/binary>>}}
                     end;
                  _@1 ->
                     case _@1 of
                        {error, _reason@1} ->
                           {halt, {error, _reason@1}};
                        _@2 ->
                           erlang:error({with_clause, _@2})
                     end
               end;
            _@1 ->
               case _@1 of
                  {error, _reason@1} ->
                     {halt, {error, _reason@1}};
                  _@2 ->
                     erlang:error({with_clause, _@2})
               end
         end
      end).

request(#{method := _method@1, path := _path@1, headers := _headers@1, body := _body@1}, #{socket := _socket@1, database := _database@1} = _state@1) ->
   #{path := _path@2, query := _query@1} = http_uri:parse(_path@1),
   {_database@3, _path@4} =
      case _path@2 of
         <<"/_db/", _rest@1/binary>> ->
            [_database@2, _path@3] = binary:split(_rest@1, <<"/">>),
            {_database@2, <<"/", _path@3/binary>>};
         _ ->
            {case _database@1 of
                _@1 when _@1 =:= false orelse _@1 =:= nil ->
                   <<>>;
                _@2 -> _@2
             end, _path@2}
      end,
   _request@1 = [1, 1, _database@3, method_for(_method@1), _path@4, query_for(_query@1), headers_for(_headers@1)],
   case eVPack:encode(_request@1) of
      {ok, _request@2} ->
         case body_for(_body@1) of
            {ok, _body@2} ->
               case send_stream(_socket@1, build_stream(<<_request@2/binary, _body@2/binary>>))
               of
                  ok ->
                     case recv_header(_socket@1) of
                        {ok, _header@1} ->
                           case recv_stream(_socket@1, _header@1) of
                              {ok, _stream@1} ->
                                 case decode_stream(_stream@1) of
                                    {ok, [[1, 2, _status@1, _headers@2] | _body@3]} ->
                                       {ok, #{status => _status@1, headers => _headers@2, body => body_from(_body@3)}, _state@1};
                                    _@3 ->
                                       case _@3 of
                                          {error, closed} ->
                                             {error, noproc, _state@1};
                                          {error, _reason@1} ->
                                             {error, _reason@1, _state@1};
                                          _@4 ->
                                             erlang:error({with_clause, _@4})
                                       end
                                 end;
                              _@3 ->
                                 case _@3 of
                                    {error, closed} ->
                                       {error, noproc, _state@1};
                                    {error, _reason@1} ->
                                       {error, _reason@1, _state@1};
                                    _@4 ->
                                       erlang:error({with_clause, _@4})
                                 end
                           end;
                        _@3 ->
                           case _@3 of
                              {error, closed} ->
                                 {error, noproc, _state@1};
                              {error, _reason@1} ->
                                 {error, _reason@1, _state@1};
                              _@4 -> erlang:error({with_clause, _@4})
                           end
                     end;
                  _@3 ->
                     case _@3 of
                        {error, closed} -> {error, noproc, _state@1};
                        {error, _reason@1} ->
                           {error, _reason@1, _state@1};
                        _@4 -> erlang:error({with_clause, _@4})
                     end
               end;
            _@3 ->
               case _@3 of
                  {error, closed} -> {error, noproc, _state@1};
                  {error, _reason@1} -> {error, _reason@1, _state@1};
                  _@4 -> erlang:error({with_clause, _@4})
               end
         end;
      _@3 ->
         case _@3 of
            {error, closed} -> {error, noproc, _state@1};
            {error, _reason@1} -> {error, _reason@1, _state@1};
            _@4 -> erlang:error({with_clause, _@4})
         end
   end.

send_stream(Socket, IoData) ->
   tcp:send(Socket, IoData).

vst_maxsize() -> 30720.
