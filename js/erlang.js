var exec = require('child_process').exec;
var http = require('http');

module.exports.connect = function(appmod, script, env, callback) {
    /* find an existing Erlang VM running in our container, and if not
       running, then start it.  this takes advantage of the optimization
       found at:

       https://aws.amazon.com/blogs/compute/container-reuse-in-lambda/

       this should work especially well with lambda functions that are
       connected to kinesis events, since they run 1-for-1 with a shard due
       to the serialization guarantees. */
    ping( appmod, function(err) {
        /* if we get a non-error response, we assume it is ready to go. */
        if( !err ) callback();
        else {
            start( appmod, script, env, callback );
        }
    });
}

module.exports.invoke = function(appmod, event, context, callback) {
    const body = JSON.stringify( { event: event, context: context } );
    const options = {
        host: 'localhost',
        port: '7181',
        path: '/erllambda/' + appmod,
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
            'Content-Length': Buffer.byteLength(body)
        }
    };
    var req = http.request( options, function(response) {
        var resp_body = '';
        response.on('data', function(chunk) {
            resp_body += chunk;
        });
        response.on('end', function() {
            if( response.statusCode == 200 ) {
                const json = JSON.parse( resp_body );
                callback( json.error, json.success );
            } else {
                callback( 'unexpected status code: ' + response.statusCode );
            }
        });
    }).on('error', function(error) {
        callback(error);
    });
    req.write( body );
    req.end();
}


function ping(appmod, callback) {
    const options = {
        host: 'localhost',
        port: '7181',
        path: '/erllambda/' + appmod,
        method: 'GET'
    };
    var req = http.request( options, function(response) {
        if( response.statusCode == 200 ) callback();
        else callback( {error: 'ping response status: '
                        + response.statusCode} );
    }).on('error', function(error) {
        callback(error);
    }).end();
}


function alive(appmod, deadline, callback) {
    const delay = 100;
    ping( appmod, function(err) {
        if( !err ) callback();
        else if( Date.now() < deadline ) {
            console.log( 'waiting %s ms for erlang start', delay );
            setTimeout( function() {
                alive(appmod, deadline, callback)
            }, delay );
        }
        else {
            callback( { action: 'alive', error: 'timeout waiting for vm' } );
        }
    });
}


function start(appmod, script, env, callback) {
    const tmp = require('tmp').dirSync();
    const glob = require('glob');
    const command = script + ' start';
    /* set some additional environment variable needed to drive rebar
       release scripts. */
    env.RELX_REPLACE_OS_VARS = "true";
    env.CORESRV_RUNDIR = tmp.name;
    env.CORESRV_PROGNAME = appmod;
    env.ERTS_DIR = glob.sync( 'erts-*' )[0];
    env.HOME = tmp.name;
    /* no matter what we just issue a stop without looking, to be safe */
    stop( script, function() {
        console.log( 'executing: "%s" with env: %s', command,
                     JSON.stringify(env) );
        exec( command, {env: env}, function(err, stdout, stderr) {
            if( err )
                callback(err);
            else {
                const deadline = Date.now() + 10000;
                alive(appmod, deadline, callback);
            }
        });
    });
}

function stop(script, callback) {
    const command = script + ' stop';
    console.log( 'executing: "%s"', command );
    exec( command, {}, function() {
        callback();
    });
}
