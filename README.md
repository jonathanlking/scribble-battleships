# Scribble Battleships

This is based on the [Purescript-Concur + Webpack starter pack](https://github.com/ajnsit/purescript-concur-webpack-starter)

## Usage

### Get the `purescript-scribble` and `scribble-java` dependencies

> git submodule update --init --recursive

### Generate the Battleships protocol

**This step is only required if you make changes to the protocol!**

First build the Scribble tool

> docker build -t scribble-java scribble-java

Out protocol is described in `Game.scr`

> docker run -v `pwd`:/repo --entrypoint "./scribblec.sh -d /repo/web/src/ /repo/Game.scr -api-ps BattleShips" scribble-java

## Server

> stack build

Start the Battleships game server

> stack exec server

## Web

### Build Purescript code

> yarn

> spago build

### Run Dev Server

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
