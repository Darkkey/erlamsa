Authors
=======

Main developer (author and maintainer)
--------------------------------------

The author of erlamsa is the main contributor and designer of the tool.

 - Alexander Bolshev aka dark_k3y 


Authors of modules / source code that exist in / used by / was used by erlamsa
-------------------------------------------------------

Erlamsa is using some external libraries and also integrates some external code. Below you could see
the full lists of such inclusions (refer to erlamsa source code for the additional references and licences):

 - original radamsa implementation was done by Aki Helen, https://gitlab.com/akihe/radamsa. Erlamsa was originally based on radamsa'2014 source code port to erlang. 
 - procket (Erlang library for socket creation and manipulation) by Michael Santos < michael.santos@gmail.com >, https://github.com/msantos/procket : used by erlamsa for raw sockets interaction
 - erlserial (Erlang library for interacting withs serial ports) originally by Johan Bevemyr, forked by Tony Garnock-Jones, https://github.com/Darkkey/erlserial : used by erlamsa for serial ports interaction
 - erlexec (Erlang library for executing and controling OS processes from Erlang/OTP) by Serge Aleynikov < saleyn@gmail.com >, https://github.com/saleyn/erlexec : used by erlamsa to implement exec:// fuzzing scheme and debug monitors
 - parts from cowhpack library (http2 state machine and http2 headers packing code) by Loïc Hoguin < essen@ninenines.eu > are used by erlamsa for http2 proxy fuzzer implementation
 - SGML parser tokenizer is built based on "trane" tokenizer by Mats Cronqvist
 - JSON parser tokenizer is partialy derived from JSONE json parser by Takeru Ohta < phjgt308@gmail.com > 
 - getopt command line parser reuses code from erlang getopt package by Juan Jose Comellas < juanjo@comellas.org >
 - payloads for JSON deserialized attacks were borrowed from 'Friday the 13th JSON Attacks' paper by Alvaro Muñoz & Oleksandr Mirosh
 - script-builder is built using parts from Chad DePue' edis ( https://github.com/cbd/edis ) installation scripts.
 - original tcp proxy implementation was borrowed from the http://beezari.livejournal.com/191194.html by beezari
 - erlamsa is using original rebar (see https://github.com/rebar/rebar) for building/testing/installing purposes.

Contributors
------------

People who contributed to erlamsa development:

- Eduardo Novella
- Boris Ryutin
- Nikita Abdullin
