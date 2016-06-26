/*jslint es6, node, white */
"use strict";
var AWS = require('aws-sdk');
var config = new AWS.Config();
var erlang = require('./erlang.js');

exports.handler = function(event, context) {
    /* compose parameter and execute the erlang handler as configured */
    var fs = require('fs');
    var handler = JSON.parse( fs.readFileSync('etc/handler.json', 'utf8') );

    /* validation of minimal handler fields */
    if( !handler ) {
        context.fail( 'handler definition invalid' ); return;
    }
    if( !handler.module ) {
        context.fail( 'handler definition missing "module"' ); return;
    }

    /* connect and/or start the erlang vm asap, since it takes a bit. */
    var env = {
        AWS_ACCESS_KEY_ID: config.credentials.accessKeyId,
        AWS_SECRET_ACCESS_KEY: config.credentials.secretAccessKey,
        AWS_SECURITY_TOKEN: config.credentials.sessionToken
    };
    erlang.connect( handler.module, handler.command, env, function(error) {
        if( error ) {
            context.fail( error ); return;
        }
        erlang.invoke( handler.module, event, context, function(error,success) {
            context.done( error, success );
        });
    });
};
