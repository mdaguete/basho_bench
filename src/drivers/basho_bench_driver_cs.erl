%% -------------------------------------------------------------------
%%
%% basho_bench: Benchmarking Suite
%%
%% Copyright (c) 2009-2010 Basho Techonologies
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(basho_bench_driver_cs).

-export([new/1,
         run/4]).

-include("basho_bench.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").


-record(state, {bucket}).

%% ====================================================================
%% API
%% ====================================================================

new(_Id) ->
	%% Ensure that erlcloud is up
	application:ensure_all_started(hackney),
	application:ensure_all_started(erlcloud),

	%% Configure erlcloud
	AccessKey = basho_bench_config:get(cs_access_key),
	SecretKey = basho_bench_config:get(cs_secret_key),
	Host      = basho_bench_config:get(cs_host, "localhost"),
	Port      = basho_bench_config:get(cs_port,  8080),
	Protocol  = basho_bench_config:get(cs_protocol, "https://"),
	Bucket    = basho_bench_config:get(cs_bucket, "bench-test"),

	erlcloud_s3:configure(AccessKey, SecretKey, Host, Port, Protocol),

	put(aws_config, #aws_config{
		access_key_id=AccessKey,
		secret_access_key=SecretKey,
		s3_host=Host,
		s3_port=Port,
		s3_scheme=Protocol,
		s3_bucket_access_method=path,
		http_client=hackney,
		hackney_pool = undefined
	}),

	erlcloud_s3:create_bucket(Bucket),

	{ok, #state {bucket=Bucket}}.


run(put, KeyGen, ValueGen, State) ->
	Key = KeyGen(),
	try

		Value = ValueGen(),
		erlcloud_s3:put_object(State#state.bucket, integer_to_list(Key), Value),
		{ok, State}
	catch
		_X:{aws_error,{http_error,404,_,_}} = _Y ->
			?ERROR("Error on PUT ~p: ~p ~p\n", [Key, _X, _Y]),
			{error, not_found, State};
		_X:{aws_error,{socket_error,timeout}} = _Y ->
			?ERROR("Error on PUT ~p: ~p ~p\n", [Key, _X, _Y]),
			{error, timeout, State}
	end;

run(get, KeyGen, _ValueGen, State) ->
	Key = KeyGen(),
	try
		erlcloud_s3:get_object(State#state.bucket, integer_to_list(Key)),
    	{ok, State}
	catch
		_X:{aws_error,{http_error,404,_,_}} = _Y ->
        ?ERROR("Error on GET ~p: ~p ~p\n", [Key, _X, _Y]),
				{error, not_found, State};
    _X:{aws_error,{socket_error,timeout}} = _Y ->
				?ERROR("Error on GET ~p: ~p ~p\n", [Key, _X, _Y]),
				{error, timeout, State}
	end;

run(delete, KeyGen, _ValueGen, State) ->
	Key = KeyGen(),
	try
		erlcloud_s3:delete_object(State#state.bucket, integer_to_list(Key)),
		{ok, State}
	catch
		_X:{aws_error,{http_error,404,_,_}} = _Y ->
			?ERROR("Error on DELETE ~p: ~p ~p\n", [Key, _X, _Y]),
			{error, not_found, State};
		_X:{aws_error,{socket_error,timeout}} = _Y ->
			?ERROR("Error on DELETE ~p: ~p ~p\n", [Key, _X, _Y]),
			{error, timeout, State}
	end.

