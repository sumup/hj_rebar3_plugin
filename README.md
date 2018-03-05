hj
=====

hello_jesse schema compiler plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { hj, ".*", {git, "git@host:user/hj.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 hj
    ===> Fetching hj
    ===> Compiling hj
    <Plugin Output>
