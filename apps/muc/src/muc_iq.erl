-module(muc_iq).
-compile([warnings_as_errors]).

-export([
    get_error/1,
    set_item/2,
    set_field/4,
    set_field/5,
    get_room_info/2,
    get_room_config/2,
    get_fields/1
]).

-include_lib("exmpp/include/exmpp.hrl").
-include("muc.hrl").

-spec get_error(atom()) -> exmpp_xml:exml().

get_error('not-acceptable') ->
    exmpp_xml:element(undefined, 'error', [
        exmpp_xml:attribute(<<"type">>, <<"modify">>), 
        exmpp_xml:attribute(<<"code">>, <<"406">>)
    ], [
        exmpp_xml:element(
            'urn:ietf:params:xml:ns:xmpp-stanzas', 
            'not-acceptable', [], []),
        exmpp_xml:element(
            'urn:ietf:params:xml:ns:xmpp-stanzas',
            'text', [], [
                exmpp_xml:cdata(<<"Wrong data in the form">>)
    ])]);

get_error('bad-request') ->
    exmpp_xml:element(undefined, 'error', [
        exmpp_xml:attribute(<<"type">>, <<"modify">>), 
        exmpp_xml:attribute(<<"code">>, <<"400">>)
    ], [
        exmpp_xml:element(
            'urn:ietf:params:xml:ns:xmpp-stanzas', 
            'bad-request', [], []),
        exmpp_xml:element(
            'urn:ietf:params:xml:ns:xmpp-stanzas',
            'text', [], [
                exmpp_xml:cdata(<<"Invalid request">>)
    ])]);

get_error('item-not-found') ->
    exmpp_xml:element(undefined, 'error', [
        exmpp_xml:attribute(<<"type">>, <<"cancel">>), 
        exmpp_xml:attribute(<<"code">>, <<"404">>)
    ], [
        exmpp_xml:element(
            'urn:ietf:params:xml:ns:xmpp-stanzas', 
            'item-not-found', [], []),
        exmpp_xml:element(
            'urn:ietf:params:xml:ns:xmpp-stanzas',
            'text', [], [
                exmpp_xml:cdata(<<"Conference room does not exist">>)
    ])]).

-spec set_item(Name :: binary(), JID :: binary()) -> exmpp_xml:xmlel().

set_item(Name, JID) ->
    exmpp_xml:element(undefined, 'item', [
        exmpp_xml:attribute(<<"name">>, Name),
        exmpp_xml:attribute(<<"jid">>, JID)
    ], []).

-spec set_field(Var :: binary(), Type :: binary() | undefined, 
    Label :: binary() | undefined, Value :: binary()) -> exmpp_xml:xmlel().

set_field(Var, Type, Label, Value) ->
    exmpp_xml:element(undefined, 'field',
        [exmpp_xml:attribute(<<"var">>, Var)] ++ 
        case Type of
            undefined -> [];
            _ -> [exmpp_xml:attribute(<<"type">>, Type)]
        end ++
        case Label of
            undefined -> [];
            _ -> [exmpp_xml:attribute(<<"label">>, Label)]
        end, [
            exmpp_xml:element(undefined, 'value', [], [
                exmpp_xml:cdata(Value)
            ])
        ]).

-type option() :: {Label :: binary(), Value :: binary()}.

-spec set_field(Var :: binary(), Type :: binary() | undefined, 
    Label :: binary() | undefined, Value :: binary(),
    Opts :: [option()]) -> exmpp_xml:xmlel().

set_field(Var, Type, Label, Value, Opts) ->
    Options = lists:map(fun({L,V}) ->
        set_option(L,V)
    end, Opts),
    exmpp_xml:element(undefined, 'field',
        [exmpp_xml:attribute(<<"var">>, Var)] ++ 
        case Type of
            undefined -> [];
            _ -> [exmpp_xml:attribute(<<"type">>, Type)]
        end ++
        case Label of
            undefined -> [];
            _ -> [exmpp_xml:attribute(<<"label">>, Label)]
        end, [
        exmpp_xml:element(undefined, 'value', [], [
            exmpp_xml:cdata(Value)]) | Options
        ]).

-spec get_room_info(RoomInfo :: room_info(), 
    RoomUsersCount :: non_neg_integer()) -> exmpp_xml:xmlel().

get_room_info(RoomInfo, RoomUsersCount) ->
    Identity = exmpp_xml:element(undefined, 'identity', [
            exmpp_xml:attribute(<<"category">>, <<"conference">>),
            exmpp_xml:attribute(<<"name">>, RoomInfo#room_info.description),
            exmpp_xml:attribute(<<"type">>, <<"text">>)
        ], []),
    Features = get_features(RoomInfo),
    Fields = lists:map(fun({Var, Type, Label, Value}) ->
        set_field(Var, Type, Label, Value)
    end, [
        {<<"FORM_TYPE">>, <<"hidden">>, none, 
            <<"http://jabber.org/protocol/muc#roominfo">>},
        {<<"muc#roominfo_description">>, none, <<"Description">>, 
            RoomInfo#room_info.description},
        {<<"muc#roominfo_changesubject">>, none, 
            <<"Occupants May Change the Subject">>, 
            change_subject_occupants(RoomInfo#room_info.change_subject)},
        % TODO: needed?
        % {<<"muc#roominfo_contactjid">>, none,
        %    <<"Contact Addresses">>, ...},
        {<<"muc#roominfo_subject">>, none,
            <<"Current Discussion Topic">>, RoomInfo#room_info.subject},
        {<<"muc#roomconfig_changesubject">>, none,
            <<"Subject can be modified">>,
            change_subject(RoomInfo#room_info.change_subject)},
        {<<"muc#roominfo_occupants">>, none,
            <<"Number of occupants">>, RoomUsersCount},
        {<<"muc#roominfo_lang">>, none,
            <<"Language of discussion">>, RoomInfo#room_info.language},
        {<<"muc#maxhistoryfetch">>, none,
            <<"Maximum Number of History Messages Returned by Room">>,
            RoomInfo#room_info.history_size}
    ]),
    Data = exmpp_xml:element('jabber:x:data', 'x', [
        exmpp_xml:attribute(<<"type">>, <<"result">>)], Fields),
    exmpp_xml:element(?NS_DISCO_INFO, 'query', [], [Data|Features] ++ [Identity]).

-spec get_room_config(RoomInfo :: room_info(), 
    RoomUsers :: [room_user()]) -> exmpp_xml:xmlel().

get_room_config(RoomInfo, _RoomUsers) ->
    Fields = lists:map(fun
        ({Var, Type, Label, Value}) ->
            set_field(Var, Type, Label, Value);
        ({Var, Type, Label, Value, Opts}) ->
            set_field(Var, Type, Label, Value, Opts)
    end, [
        {<<"FORM_TYPE">>, <<"hidden">>, none, 
            <<"http://jabber.org/protocol/muc#roomconfig">>},
        {<<"muc#roomconfig_roomname">>, <<"text-single">>, 
            <<"Room title">>, 
            string(RoomInfo#room_info.subject)},
        {<<"muc#roomconfig_roomimage">>, <<"text-single">>, 
            <<"Room image">>, 
            string(RoomInfo#room_info.image)},
        {<<"muc#roomconfig_roomdesc">>, <<"text-single">>, 
            <<"Room description">>, 
            string(RoomInfo#room_info.description)},
        {<<"muc#roomconfig_persistentroom">>, <<"boolean">>, 
            <<"Make room persistent">>, 
            bool_to_bin(RoomInfo#room_info.persistent)},
        {<<"muc#roomconfig_publicroom">>, <<"boolean">>, 
            <<"Make room public searchable">>, 
            bool_to_bin(RoomInfo#room_info.public)},
        {<<"public_list">>, <<"text-single">>, 
            <<"Make participants list public">>, 
            bool_to_bin(RoomInfo#room_info.public_occupants)},
        {<<"muc#roomconfig_roomsecret">>, <<"text-private">>, 
            <<"Password">>, 
            string(RoomInfo#room_info.password)},
        {<<"muc#roomconfig_passwordprotectedroom">>, <<"boolean">>, 
            <<"Make room password protected">>, 
            has_password(RoomInfo#room_info.password)},
        {<<"muc#roomconfig_maxusers">>, <<"list-single">>, 
            <<"Maximum Number of Occupants">>, 
            int_to_bin(RoomInfo#room_info.max_users), [
                {<<"5">>, <<"5">>},
                {<<"10">>, <<"10">>},
                {<<"20">>, <<"20">>},
                {<<"30">>, <<"30">>},
                {<<"50">>, <<"50">>},
                {<<"100">>, <<"100">>},
                {<<"200">>, <<"200">>}
            ]},
        {<<"muc#roomconfig_whois">>, <<"list-single">>, 
            <<"Present real Jabber IDs to">>, 
            real_jids(RoomInfo#room_info.real_jids), [
                {<<"moderators only">>, <<"moderators">>},
                {<<"anyone">>, <<"anyone">>}
            ]},
        {<<"muc#roomconfig_membersonly">>, <<"boolean">>,
            <<"Make room members-only">>,
            bool_to_bin(RoomInfo#room_info.members_only)},
        {<<"muc#roomconfig_moderatedroom">>, <<"boolean">>,
            <<"Make room moderated">>,
            bool_to_bin(RoomInfo#room_info.moderated_room)},
        {<<"members_by_default">>, <<"boolean">>,
            <<"Default users as participants">>,
            bool_to_bin(RoomInfo#room_info.members_by_default)},
        {<<"muc#roomconfig_changesubject">>, <<"boolean">>,
            <<"Allow users to change the subject">>,
            change_subject_occupants(RoomInfo#room_info.change_subject)},
        {<<"allow_private_messages">>, <<"boolean">>,
            <<"Allow users to send private messages">>,
            bool_to_bin(RoomInfo#room_info.allow_private_messages)},
        {<<"allow_private_messages_from_visitors">>, <<"list-single">>,
            <<"Allow visitors to send private messages to">>,
            allow_private_messages_from_visitors(
                RoomInfo#room_info.allow_private_messages_from_visitors), [
                {<<"nobody">>, <<"nobody">>},
                {<<"moderators only">>, <<"moderators">>},
                {<<"anyone">>, <<"anyone">>}
            ]},
        {<<"allow_query_users">>, <<"boolean">>,
            <<"Allow users to query other users">>,
            bool_to_bin(RoomInfo#room_info.allow_query_users)},
        {<<"muc#roomconfig_allowinvites">>, <<"boolean">>,
            <<"Allow users to send invites">>,
            bool_to_bin(RoomInfo#room_info.allow_invites)},
        {<<"muc#roomconfig_allowvisitorstatus">>, <<"boolean">>,
            <<"Allow visitors to send status text in presence updates">>,
            bool_to_bin(RoomInfo#room_info.allow_visitors_status)},
        {<<"muc#roomconfig_allowvisitorsnickchange">>, <<"boolean">>,
            <<"Allow visitors to change nickname">>,
            bool_to_bin(RoomInfo#room_info.allow_visitors_change_nickname)},
        {<<"muc#roomconfig_allowvoicerequests">>, <<"boolean">>,
            <<"Allow visitors to send voice requests">>,
            bool_to_bin(RoomInfo#room_info.allow_visitors_voice_requests)},
        {<<"muc#roomconfig_voicerequestmininterval">>, <<"text-single">>,
            <<"Minimum interval between voice requests (in seconds)">>,
            RoomInfo#room_info.voice_request_min_interval},
        {<<"muc#roomconfig_mainowner">>, <<"jid-multi">>,
            <<"Main Owner">>,
            RoomInfo#room_info.main_owner},
        %% TODO: send all of owners from RoomUsers
        {<<"muc#roomconfig_roomowners">>, <<"jid-multi">>,
            <<"Full List of Room Owner">>,
            <<>>},
        %% TODO: send all of white list JIDs from RoomUsers
        {<<"muc#roomconfig_captcha_whitelist">>, <<"jid-multi">>,
            <<"Exclude Jabber IDs from CAPTCHA challenge">>,
            <<>>}
    ]),
    Data = exmpp_xml:element('jabber:x:data', 'x', [
        exmpp_xml:attribute(<<"type">>, <<"form">>)], Fields),
    exmpp_xml:element(?NS_DISCO_INFO, 'query', [], [Data]).

-spec get_fields(IQ :: exmpp_xml:exml()) -> [option()].

get_fields(IQ) ->
    X = exmpp_xml:get_path(IQ, [{element, 'query'}, {element, 'x'}]),
    lists:flatmap(fun
        (#xmlel{name='field'}=Xmlel) ->
            Var = exmpp_xml:get_attribute(Xmlel, <<"var">>, undefined),
            Val = exmpp_xml:get_path(Xmlel, [{element, 'value'}, cdata]),
            [{Var, Val}];
        (Xmlel) ->
            lager:debug("Ignoring ~p~n", [Xmlel]),
            []
    end, exmpp_xml:get_child_elements(X)).

%% -----------------------------------------------------------------------
%% Internal functions

-spec string(binary() | undefined) -> binary().

string(undefined) -> <<>>;
string(Any) -> Any.

-spec has_password(undefined | binary()) -> boolean().

has_password(undefined) -> false;
has_password(<<>>) -> false;
has_password(_) -> true.

-spec change_subject_occupants(change_subject()) -> binary().

change_subject_occupants(all) -> <<"true">>;
change_subject_occupants(_) -> <<"false">>.

-spec change_subject(change_subject()) -> binary().

change_subject(none) -> <<"false">>;
change_subject(_) -> <<"true">>.

-spec real_jids(moderators | anyone) -> binary().

real_jids(moderators) -> <<"moderators">>;
real_jids(anyone) -> <<"anyone">>.

-spec allow_private_messages_from_visitors(atom()) -> binary().

allow_private_messages_from_visitors(nobody) -> <<"nobody">>;
allow_private_messages_from_visitors(moderators) -> <<"moderators">>;
allow_private_messages_from_visitors(anyone) -> <<"anyone">>.

-spec bool_to_bin(boolean()) -> binary().

bool_to_bin(true) -> <<"1">>;
bool_to_bin(false) -> <<"0">>.

-spec int_to_bin(integer()) -> binary().

int_to_bin(Integer) ->
    list_to_binary(integer_to_list(Integer)).

-spec set_option(Label :: binary(), Value :: binary()) -> exmpp_xml:exml().

set_option(Label, Value) ->
    exmpp_xml:element(undefined, 'option', [
        exmpp_xml:attribute(<<"label">>, Label)
    ], [
        exmpp_xml:element(undefined, 'value', [], [
            exmpp_xml:cdata(Value)
        ])
    ]).

-spec get_features(RoomInfo :: room_info()) -> [exmpp_xml:exml()].

get_features(_RoomInfo) ->
    % TODO: add features (when it's supported) as:
    % muc_passwordprotected
    % muc_hidden
    % muc_temporary
    % muc_open
    % muc_unmoderated
    % muc_nonanonymous
    lists:map(fun(Feature) ->
        exmpp_xml:element(undefined, 'feature', [
            exmpp_xml:attribute(<<"var">>, Feature)], [])
    end, [
        <<"http://jabber.org/protocol/muc">>
    ]).
