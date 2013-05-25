#lang scribble/manual

@require[(for-label dbus/proxy)
         (for-label racket)]

@title{Proxies}
@defmodule[dbus/proxy]

In order to simplify working with objects, you can construct so-called
proxies that hold most of the annoying @racket[dbus-call] parameters and
even take care about signatures.


@defstruct[dbus-member ((bus        dbus-connection?)
                        (endpoint   dbus-bus-name?)
                        (path       dbus-object-path?)
                        (interface  dbus-interface-name?)
                        (name       dbus-member-name?)
                        (signature  dbus-signature?))
                       #:transparent]{
 Generic object member description.
}

@defstruct[(dbus-method dbus-member) () #:transparent]{
 A member method.
}

@defproc[(dbus-method-call (method dbus-method?)
                           (argument any/c) ...) any]{
 Call proxied method with specified arguments.
}

@defproc[(dbus-method-send (method dbus-method?)
                           (argument any/c) ...) any]{
 Send message without a reply using a proxy method.
}


@defstruct[(dbus-property dbus-member) () #:transparent]{
 A member property.
}

@defproc[(dbus-property-get (property dbus-property?)) any]{
 Retrieve value of proxied property.
}

@defproc[(dbus-property-set! (property dbus-property?)
                             (value any/c) ...) any]{
 Set value of proxied property.
}

@defstruct[(dbus-signal dbus-member) () #:transparent]{
 A member signal.
}

@defstruct[dbus-object ((bus         dbus-connection?)
                        (endpoint    dbus-bus-name?)
                        (path        dbus-object-path?)
                        (methods     (hash/c dbus-member-name? dbus-method?))
                        (properties  (hash/c dbus-member-name? dbus-property?))
                        (signals     (hash/c dbus-member-name? dbus-signal?)))
                       #:transparent]{
 Proxy of the whole object with methods, properties and signals.
}


@defproc[(dbus-object-call (object dbus-object?)
                           (method dbus-method-name?)
                           (argument any/c) ...) any]{
 Call method of a proxied object.
}

@defproc[(dbus-object-send (object dbus-object?)
                           (method dbus-method-name?)
                           (argument any/c) ...) any]{
 Send message to a proxied object.
}

@defproc[(dbus-object-get (object dbus-object?)
                          (property dbus-member-name?)) any]{
 Retrieve value of proxied objects's property.
}

@defproc[(dbus-object-set! (object dbus-object?)
                           (property dbus-member-name?)
                           (value any/c) ...) any]{
 Set value of proxied object's property.
}


@; vim:set ft=scribble sw=2 ts=2 et:
