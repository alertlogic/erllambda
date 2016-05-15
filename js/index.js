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
        }
    });
}

/* normally lambda will call our handlers to drive things, but we are are
   just testing standalone then this will drive things. */
if (!module.parent) {
    var event = { foo: true };
    var context = { bar: true };
    context.done = function(error, succeed) {
        console.log( 'error: ' + error )
        console.log( 'succeed: ' + succeed )
    };
    context.succeed = function(string) { console.log(string) };
    context.fail = function(string) { console.log(string) };

    console.log( 'starting test!!!!' );
    console.log( 'event: ' + JSON.stringify(event) );
    console.log( 'context: ' + JSON.stringify(context) );
    exports.elHandler(event, context);
}
