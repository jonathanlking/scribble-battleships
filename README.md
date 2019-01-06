# Scribble Battleships

This is based on the [Purescript-Concur + Webpack starter pack](https://github.com/ajnsit/purescript-concur-webpack-starter)

## Usage

### Generate the Battleships protocol

TODO

Protocol is described in `Game.scr`

### Get the purescript-scribble dependency

> git submodule update --init --recursive

## Server

> stack build

Start the Battleships game server

> stack exec server

## Web

### Build Purescript code

> bower install

> pulp build

### Run Dev Server

> yarn

> yarn start

## Hot code reload with purescript code

At the end of the previous command, you will have a development server
which will watch for changes, and automatically reload the web page.
This mechanism only works with JS changes.

However, in practice, your IDE should automatically recompile Purescript to
Javascript on every change, which will be picked up by the development server.
So you get immediate recompilation even with Purescript.

### Build production artifacts

> yarn build
