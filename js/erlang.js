/*jslint es6, node, white */
"use strict";
var async = require('async');
var exec = require('child_process').exec;
var http = require('http');

function ping(appmod, callback) {
    const options = {
        socketPath: '/tmp/erllambda.sock'
        host: 'localhost',
        path: '/erllambda/' + appmod,
        method: 'GET'
    };
    http.request( options, function(response) {
        if( response.statusCode === 200 ) { callback(); }
        else {
            callback( {error: 'ping response status: '
                       + response.statusCode} );
        }
    }).on('error', function(error) {
        callback(error);
    }).end();
}


function alive(appmod, deadline, callback) {
    const delay = 100;
    ping( appmod, function(err) {
        if( !err ) {
            console.log( 'erlang alive: success', delay );
            callback();
        }
        else if( Date.now() < deadline ) {
            setTimeout( function() {
                alive(appmod, deadline, callback);
            }, delay );
        }
        else {
            console.log( 'erlang alive: deadline exceeded' );
            callback( err );
        }
    });
}


function stop(script, callback) {
    const command = script + ' stop';
    console.log( 'executing: "%s"', command );
    exec( command, {}, function() {
        callback();
    });
}


function start(appmod, script, env, callback) {
    const fs = require('fs');
    const command = script + ' start';
    const taskdir = '/var/task';
    var releasedir = null;
    var rundir = null;

    /* create the temp directory and move copies of the config files into it
       so they are writable by the erlang node script. */
    async.series( [
        function(callback) {
            async.parallel( [
                function(callback) {
                    const tmp = require('tmp');
                    tmp.dir(function(err, path) {
                        if(!err) { rundir = path; }
                        callback(err);
                    });
                },
                function(callback) {
                    const glob = require('glob');
                    glob( 'releases/*.*.*', function(err, matches) {
                        if(!err) { releasedir = matches[0]; }
                        callback(err);
                    });
                }
            ], callback );
        },
        function(callback) {
            async.forEach(
                ['vm.args', 'sys.config'],
                function(name, callback) {
                    var source = taskdir + '/' + releasedir + '/' + name;
                    var dest = rundir + '/' + name;
                    console.log( 'linking ' + dest + ' -> ' + source );
                    fs.symlink( source, dest, function(err) {
                        if(err) {console.log( 'link failed: ' + err );}
                        callback(err);
                    });
                }, callback );
        },
        function(callback) {
            async.forEach(
                ['cachefs', 'checkpointfs', 'tmpfs', 'ramfs'],
                function(name, callback) {
                    const dirname = rundir + '/' + name;
                    console.log( 'creating dir: ' + dirname );
                    fs.mkdir( dirname, function(err) {
                        if(err && err.code !== 'EEXISTS') {
                            console.log( 'create dir failed: ' + err );
                        }
                        callback(err);
                    });
                }, callback );
        },
        function(callback) {
            /* set some additional environment variable needed to drive rebar
               release scripts. */
            env.RELX_REPLACE_OS_VARS = "true";
            env.VAR_DIR = rundir;
            env.RUN_DIR = rundir;
            env.RELX_CONFIG_PATH = rundir + '/sys.config';
            env.VMARGS_PATH = rundir + '/vm.args';
            env.RUNNER_LOG_DIR = rundir + '/log';
            env.PROGNAME = appmod;
            env.HOME = taskdir;
            /* no matter what we just issue a stop without looking, to be
               safe */
            stop( script, function() {
                console.log( 'executing: "%s" with env: %s', command,
                             JSON.stringify(env) );
                exec( command, {env: env}, function(err) {
                    if( err ) { callback(err); }
                    else {
                        const deadline = Date.now() + 10000;
                        alive(appmod, deadline, callback);
                    }
                });
            });
        }
    ], callback );
}


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
        if( !err ) { callback(); }
        else {
            start( appmod, script, env, callback );
        }
    });
};

module.exports.invoke = function(appmod, event, context, callback) {
    const body = JSON.stringify( { event: event, context: context } );
    const options = {
        socketPath: '/tmp/erllambda.sock'
        host: 'localhost',
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
            if( response.statusCode === 200 ) {
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
};
