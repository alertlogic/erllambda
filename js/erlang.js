/*jslint es6, node, white */
"use strict";
const async = require('async');
const spawn = require('child_process').spawn;
const spawnSync = require('child_process').spawnSync;
const http = require('http');
const fs = require('fs');
const glob = require('glob');
const future = require('future');
var flushFuture = null;

function APIError(json) {
    if( json && json.errorType ) {
        this.name = json.errorType;
        this.message = json.errorMessage;
    } else {
        this.name = "InvalidResponse";
        this.message = "request failure, with unexpected response";
    }
}
APIError.prototype = new Error();

function ProcessError(code) {
    this.name = "ProcessError";
    this.message = `processed exited abnormally with status ${code}`;
}
ProcessError.prototype = new Error();

function ping(appmod, callback) {
    const options = {
        socketPath: '/tmp/eeecomm.sock',
        host: 'localhost',
        path: '/eee/v1/' + appmod,
        method: 'GET'
    };
    http.request( options, function(response) {
        if( response.statusCode === 200 ) { callback(); }
        else {
            var resp_body = '';
            response.on('data', function(chunk) {
                resp_body += chunk;
            });
            response.on('end', function() {
                const json = body === '' ? null : JSON.parse( resp_body );
                callback( new APIError(json) );
            });
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


function output( data ) {
    const eof = 'EOF: ';
    const messages = data.toString().split('\t\n');
    messages.forEach( function(line) {
        if( line !== '' ) {
            if( flushFuture && line.lastIndexOf(eof, 0) === 0 ) {
                flushFuture.deliver( null, "complete" );
            } else {
                console.log( line );
            }
        }
    });
}

function start(appmod, script, env, callback) {
    const taskdir = '/var/task';
    var releasedir = null;
    var rundir = '/tmp/erllambda_rundir';
    var libdir = null;

    /* create the temp directory and move copies of the config files into it
       so they are writable by the erlang node script. */
    async.series( [
        function(callback) {
            async.parallel( [
                function(callback) {
                    console.log( 'creating dir: ' + rundir );
                    fs.mkdir( rundir, function(err) {
                        if(err) {
                            if (err.code !== 'EEXIST') {
                                console.log( 'create dir failed: ' + err );
                            } else {
                                /* If dir exists its because erlang crashed
                                   and we are restarting the erlang vm;
                                   cleanup and recreate temp folder */
                                console.log( 'recreating rundir: ' + rundir);
                                spawnSync('rm', ['-rf', rundir]);
                                fs.mkdirSync(rundir);
                                err = null;
                            }
                        }
                        callback(err);
                    });
                },
                function(callback) {
                    glob(taskdir +  '/erts-*/lib', function(err, matches) {
                        if (!err) {
                            libdir = matches[0];
                        }
                        callback(err);
                    });
                },
                function(callback) {
                    glob( 'releases/*.*.*', function(err, matches) {
                        if(!err) { releasedir = matches[0]; }
                        callback(err);
                    });
                }
            ], callback );
        },
        function(callback) {
            async.forEach(
                ['cachefs', 'checkpointfs', 'tmpfs', 'ramfs'],
                function(name, callback) {
                    const dirname = rundir + '/' + name;
                    console.log( 'creating dir: ' + dirname );
                    fs.mkdir( dirname, function(err) {
                        if(err && err.code !== 'EEXIST') {
                            console.log( 'create dir failed: ' + err );
                        }
                        callback(err);
                    });
                }, callback );
        },
        function(callback) {
            /* set some additional environment variable needed to drive rebar
               release scripts. */
            env.NATIVELIB_DIR = libdir;
            env.VAR_DIR = rundir;
            env.RUN_DIR = rundir;
            env.PROGNAME = appmod;
            env.ERL_CRASH_DUMP = "/dev/null";
            /* no matter what we just issue a stop without looking, to be
               safe */
            console.log( 'executing: "%s" with env: %s', script,
                         JSON.stringify(env) );
            const child = spawn( script, [], {env: env} );
            child.stdout.on('data', output );
            child.stderr.on('data', output );
            child.on('close', (code) => {
                console.log( '%s executed with %d', script, code );
                if (code > 0) {
                    callback(new ProcessError(code));
                }
            });

            const deadline = Date.now() + 10000;
            alive( appmod, deadline, callback );
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
        socketPath: '/tmp/eeecomm.sock',
        host: 'localhost',
        path: '/eee/v1/' + appmod,
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
            'Content-Length': Buffer.byteLength(body)
        }
    };
    flushFuture = new future.create();
    var req = http.request( options, function(response) {
        var resp_body = '';
        response.on('data', function(chunk) {
            resp_body += chunk;
        });
        response.on('end', function() {
            const json = resp_body === '' ? null : JSON.parse( resp_body );
            flushFuture.when( function() {
                if( response.statusCode === 200 ) {
                    callback( null, json.success );
                } else {
                    callback( new APIError(json) );
                }
                flushFuture = null;
            });
        });
    }).on('error', function(error) {
        callback(error);
    });
    req.on('socket',function(socket){
        // give erlang side the same time for respond
        socket.setTimeout(context.getRemainingTimeInMillis(), function(){
            req.abort();  //causes error event ↑
        });
    });
    req.write( body );
    req.end();
};
