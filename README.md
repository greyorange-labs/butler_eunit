butler_eunit
=====

eunit setup for butler server

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {butler_eunit, {git, "https://host/user/butler_eunit.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 butler_eunit
    ===> Fetching butler_eunit
    ===> Compiling butler_eunit
    <Plugin Output>
