%%%-------------------------------------------------------------------
%%% File        : mod_mam_export.erl
%%% Author      : Lamtei W <lamteiwahlang@gmail.com>
%%% Description : Export archive preferences and messages from mnesia internal
%%%               database to MySQL database.
%%%
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%-------------------------------------------------------------------

-module(mod_mam_export).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").
-include("ejabberd_sql_pt.hrl").
-include("ejabberd_commands.hrl").

-compile([{parse_transform, ejabberd_sql_pt}]).

-behaviour(gen_mod).

-record(archive_msg,
	{us = {<<"">>, <<"">>}                	:: {binary(), binary()} | '$2',
	 id = <<>>                            	:: binary() | '_',
	 timestamp = p1_time_compat:timestamp() :: erlang:timestamp() | '_' | '$1',
	 peer = {<<"">>, <<"">>, <<"">>}      	:: ljid() | '_' | '$3' | undefined,
	 bare_peer = {<<"">>, <<"">>, <<"">>} 	:: ljid() | '_' | '$3',
	 packet = #xmlel{}                    	:: xmlel() | message() | '_',
	 nick = <<"">>                        	:: binary(),
	 type = chat                          	:: chat | groupchat}).

-record(archive_prefs,
	{us = {<<"">>, <<"">>} :: {binary(), binary()},
	 default = never       :: never | always | roster,
	 always = []           :: [ljid()],
	 never = []            :: [ljid()]}).

%% API
-export([depends/2, export_mam_archive/1, export_mam_archive_prefs/1,
	 mod_opt_type/1, start/2, stop/1]).

-define(PROCNAME, ?MODULE).

start(_Host, _Opts) ->
    ejabberd_commands:register_commands(command_api()),
    ?INFO_MSG("Mod mam export host: ~p~n", [_Host]),
    ok.

stop(_Host) ->
    ejabberd_commands:unregister_commands(command_api()),
    ok.

export_mam_archive_prefs(_Host) ->
    mnesia:activity(sync_dirty,
		fun () ->
			mnesia:foldl(fun (#archive_prefs{us =
					{LUser, LServer},
					default = Default,
					always = Always,
					never = Never},
				Acc) ->
				Acc,
				mysql_store_archive_prefs(LUser,
					LServer, Default,
					Always, Never)
	    	end,
			 ignored_acc, archive_prefs)
		end),
    {res, 0}.

export_mam_archive(_Host) ->
    mnesia:activity(sync_dirty,
		fun () ->
			mnesia:foldl(fun (#archive_msg{us =
				   {LUser, LServer},
				   id = ID,
				   timestamp = TS,
				   peer = LPeer,
				   type = Type,
				   nick = Nick,
				   packet = Pkt},
				Acc) ->
				Acc,
				mysql_store_archive(Pkt, LServer,
					 {LUser, LServer},
					 ID, TS, Type,
					 LPeer, Nick)
			 end,
			 ignored_acc, archive_msg)
		end),
    {res, 0}.

mysql_store_archive(Pkt, LServer, {LUser, LHost}, ID, TS, Type,
	    Peer, Nick) ->
    TStmp = now_to_usec(TS),
    SUser = case Type of
	      chat -> LUser;
	      groupchat -> jid:to_string({LUser, LHost, <<>>})
	    end,
    BarePeer =
	jid:to_string(jid:tolower(jid:remove_resource(Peer))),
    LPeer = jid:to_string(jid:tolower(Peer)),
    XML = fxml:element_to_binary(Pkt),
    Body = fxml:get_subtag_cdata(Pkt, <<"body">>),
    SType = jlib:atom_to_binary(Type),
    case ejabberd_sql:sql_query(
			LServer,
			?SQL("INSERT INTO archive (username, timestamp, "
				 "peer, bare_peer, xml, txt, kind, nick) "
				 "values (%(SUser)s, %(TStmp)d, %(LPeer)s, "
				 "%(BarePeer)s, %(XML)s, %(Body)s, %(SType)s, "
				 "%(Nick)s)")) 	of
      {updated, _} ->
	  	?INFO_MSG("Inserted record with ID ~p~n", [ID]),
	  	{res, 0};
      Err -> Err,
	  	?ERROR_MSG("Error ~p~n", [Err]),
		{res, 1}
    end.


mysql_store_archive_prefs(LUser, LServer, Default,
					   Always, Never) ->
    SDefault = erlang:atom_to_binary(Default, utf8),
    SAlways = misc:term_to_expr(Always),
    SNever = misc:term_to_expr(Never),
    case ejabberd_sql:sql_query(
            LServer,
			?SQL("INSERT INTO archive_prefs (username, def, always, never) values"
			"(%(LUser)s, %(SDefault)s, %(SAlways)s, %(SNever)s)")) of
	{updated, _} ->
	  	?INFO_MSG("Archive pref: default: ~p, always: ~p, never: ~p~n", [Default, Always, Never]),
	  	{res, 0};
	Err -> Err,
	  	?ERROR_MSG("Error ~p~n", [Err]),
		{res, 1}
    end.

%% ---------------------
%% ejabberd commands
%% ---------------------

command_api() ->
    [#ejabberd_commands{name = export_mam_archive,
			tags = [export_mam_archive],
			desc =
			    "Export all MAM archive messages from "
			    "internal mnesia DB to SQL",
			longdesc =
			    "Export all MAM archive messages from "
			    "internal mnesia DB to SQL. Depending "
			    "on the size of your database, this might "
			    "take considerable amount of time",
			module = ?MODULE, function = export_mam_archive,
			args = [{host, binary}], result = {res, rescode}},

    #ejabberd_commands{name = export_mam_archive_prefs,
			tags = [export_mam_archive_prefs],
			desc =
			    "Export all MAM archive preferences from "
			    "internal mnesia DB to SQL",
			module = ?MODULE, function = export_mam_archive_prefs,
			args = [{host, binary}], result = {res, rescode}}].

%%%===================================================================
%%% Internal functions
%%%===================================================================

now_to_usec({MSec, Sec, USec}) ->
    (MSec * 1000000 + Sec) * 1000000 + USec.

mod_opt_type(_Opt) ->
	ok.

depends(_Host, _Opts) ->
	[].
