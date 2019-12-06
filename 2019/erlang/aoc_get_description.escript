#!/usr/bin/env escript

main([Day]) ->
    Url = "https://adventofcode.com/2019/day/",
    [DayValue] = Day,
    Cookie = "",
    ok = application:start(asn1),
    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(inets),
    {ok, {_, _, WebResult}} = httpc:request(get, {Url ++ Day, [{"cookie", Cookie}]}, [{ssl,[{verify,0}]}], [{body_format, binary}]),
    [_, RemovedHeadResult] = binary:split(WebResult, <<"Day ",  DayValue, ": ">>),
    [Title, TitleRemovedResult] = binary:split(RemovedHeadResult, <<" ---</h2>">>),
    [Content, ContentRest] = binary:split(TitleRemovedResult, <<"\n</article>">>),
    {FinalResult, <<>>} = parse_html(Content),
    io:format("# Day ~s: ~s~n", [Day, Title]),
    io:format("~s~n", [FinalResult]),
    try
        [_, TitleRemovedResult2] = binary:split(ContentRest, <<" ---</h2>">>),
        [Content2, _] = binary:split(TitleRemovedResult2, <<"\n</article>">>),
        {FinalResult2, <<>>} = parse_html(Content2),
        io:format("# ~s~n", ["Part Two"]),
        io:format("~s~n", [FinalResult2])
    catch _:_ ->
        ok
    end.

parse_html(<<Input/binary>>) ->
    parse_html(Input, undefined_tag, <<>>).

parse_html(<<>>, _, Acc) ->
    {Acc, <<>>};
parse_html(<<"</p>", Rest/binary>>, p_tag, <<Acc/binary>>) ->
    {Acc, Rest};
parse_html(<<"<p>", Rest/binary>>, Tag, Acc) ->
    {InnerResult, InnerRest} = parse_html(Rest, p_tag, <<>>),
    parse_html(InnerRest, Tag, <<Acc/binary, InnerResult/binary, "\n">>);
parse_html(<<"</em>", Rest/binary>>, em_tag, <<Acc/binary>>) ->
    {Acc, Rest};
parse_html(<<"<em>", Rest/binary>>, p_tag, Acc) ->
    {InnerResult, InnerRest} = parse_html(Rest, em_tag, <<>>),
    parse_html(InnerRest, p_tag, <<Acc/binary, "**", InnerResult/binary, "**">>);
parse_html(<<"<em>", Rest/binary>>, Tag, Acc) ->
    {InnerResult, InnerRest} = parse_html(Rest, em_tag, <<>>),
    parse_html(InnerRest, Tag, <<Acc/binary, InnerResult/binary>>);
parse_html(<<"</ul>", Rest/binary>>, ul_tag, <<Acc/binary>>) ->
    {Acc, Rest};
parse_html(<<"<ul>", Rest/binary>>, Tag, Acc) ->
    {InnerResult, InnerRest} = parse_html(Rest, ul_tag, <<>>),
    parse_html(InnerRest, Tag, <<Acc/binary, InnerResult/binary>>);
parse_html(<<"</li>", Rest/binary>>, li_tag, <<Acc/binary>>) ->
    {Acc, Rest};
parse_html(<<"<li>", Rest/binary>>, ul_tag, Acc) ->
    {InnerResult, InnerRest} = parse_html(Rest, li_tag, <<>>),
    parse_html(InnerRest, ul_tag, <<Acc/binary, "* ", InnerResult/binary>>);
parse_html(<<"</code></pre>", Rest/binary>>, pre_code_tag, <<Acc/binary>>) ->
    {Acc, Rest};
parse_html(<<"<pre><code>", Rest/binary>>, Tag, Acc) ->
    {InnerResult, InnerRest} = parse_html(Rest, pre_code_tag, <<>>),
    parse_html(InnerRest, Tag, <<Acc/binary, "```\n", InnerResult/binary, "\n```">>);
parse_html(<<"</code>", Rest/binary>>, code_tag, <<Acc/binary>>) ->
    {Acc, Rest};
parse_html(<<"<code>", Rest/binary>>, Tag, Acc) ->
    {InnerResult, InnerRest} = parse_html(Rest, code_tag, <<>>),
    parse_html(InnerRest, Tag, <<Acc/binary, "`", InnerResult/binary, "`">>);
parse_html(<<"</a>", Rest/binary>>, a_tag, <<Acc/binary>>) ->
    {Acc, Rest};
parse_html(<<"<a href=\"", Rest/binary>>, Tag, Acc) ->
    [Link, <<$>, SplitRest/binary>>] = binary:split(Rest, <<$">>),
    {InnerResult, InnerRest} = parse_html(SplitRest, a_tag, <<>>),
    parse_html(InnerRest, Tag, <<Acc/binary, "[", InnerResult/binary, "]", "(", Link/binary, ")">>);
parse_html(<<"\n", Rest/binary>>, Tag, <<Acc/binary>>) ->
    parse_html(Rest, Tag, <<Acc/binary, "\n">>);
parse_html(<<"<br/>", Rest/binary>>, Tag, <<Acc/binary>>) ->
    parse_html(Rest, Tag, <<Acc/binary, "`\n`">>);
parse_html(<<C, Rest/binary>>, Tag, <<Acc/binary>>) ->
    parse_html(Rest, Tag, <<Acc/binary, C>>).

