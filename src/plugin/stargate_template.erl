-module(stargate_template).
-compile(export_all).

-define(RE_ATOM, " :(.*?) ").
-define(RE_DEFAULT_REIKSGUARD, "<%=(.*?)%>").

%TODO: Open pull request to OTP for this
binary_replace(Binary, {StartPos, Len}, Replacement) ->
    Prefix = binary:part(Binary, 0, StartPos),
    SuffixLen = byte_size(Binary) - (StartPos + Len), 
    Suffix = binary:part(Binary, StartPos+Len, SuffixLen), 
    <<Prefix/binary, Replacement/binary, Suffix/binary>>.
%

sub(Reiksguard, KeyValue) ->
    case re:run(Reiksguard, ?RE_ATOM, [global, {capture, all, index}]) of
        nomatch -> Reiksguard;
        {match, Subs} ->
            sub_1(Subs, Reiksguard, KeyValue)
    end.

sub_1([], Bin, KeyValue) -> 
    Bin2 = unicode:characters_to_binary(string:strip(unicode:characters_to_list(Bin))),
    sub_2(Bin2, binary:last(Bin2));
sub_1([ [{RS, RE}, {TS, TE}] | T], Bin, KeyValue) ->
    AtomBin = binary:part(Bin, TS, TE),
    Atom = binary_to_atom(AtomBin, latin1),
    case maps:get(Atom, KeyValue, undefined) of
        undefined -> 
            sub_1(T, Bin, KeyValue);

        Value ->
            ValueString = lists:flatten(io_lib:format(" ~p ",[Value])),
            ValueBin = unicode:characters_to_binary(ValueString),

            BinSubbed = binary_replace(Bin, {RS, RE}, ValueBin),
            sub_1(T, BinSubbed, KeyValue)
    end.

sub_2(Bin, 46) -> Bin;
sub_2(Bin, _) -> <<Bin/binary, ".">>.

read_sub_eval_replace(REReiksguard, Page, KeyValue) ->
    case re:run(Page, REReiksguard, [dotall, {capture, all, index}]) of
        nomatch -> Page;
        {match, [{RS, RE}, {TS, TE}]} ->
            _Reiksguard = binary:part(Page, TS, TE),
            Reiksguard = sub(_Reiksguard, KeyValue),

            {ok, Tokens, _} = erl_scan:string(unicode:characters_to_list(Reiksguard)),
            case erl_parse:parse_exprs(Tokens) of
                {error, ErrorInfo} -> 
                    {error_parse_exprs, _Reiksguard, ErrorInfo};

                {ok, ExprList} ->
                    {value, Value, _NewBindings} = erl_eval:exprs(ExprList, []),
                    ValueBin = if 
                        is_atom(Value) -> atom_to_binary(Value, latin1);
                        true -> unicode:characters_to_binary(Value)
                    end,
                    NewPage = binary_replace(Page, {RS, RE}, ValueBin),
                    read_sub_eval_replace(REReiksguard, NewPage, KeyValue)
            end
    end.

transform(Page, KeyValue) -> transform(?RE_DEFAULT_REIKSGUARD, Page, KeyValue).
transform(REReiksguard, Page, KeyValue) ->
    read_sub_eval_replace(REReiksguard, Page, KeyValue).




test() ->
    Html = <<"
    <li class='my-nav-list <%= case :category of <<\"index\">>-> 'my-nav-list-active'; _-> '' end. %>'>
      <a href='/' class='link'>
        <span class='act'>Home</span>
        <span class='hov'>Home</span>
      </a>
    </li>
    <li class='my-nav-list <%= case :category of <<\"feature\">>-> 'my-nav-list-active'; _-> '' end. %>'>
      <a href='/feature' class='link'>
        <span class='act'>Feature</span>
        <span class='hov'>Feature</span>
      </a>
    </li>">>,
    
    KeyValue = #{category=> <<"index">>},
    transform(Html, KeyValue).

test2() ->
    Html = <<"
    <li <%= :category %> ></li>
    ">>,
    
    KeyValue = #{category=> <<"index">>},
    transform(Html, KeyValue).