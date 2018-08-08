# Purescript-Concur-Webpack-Starter

A Starter kit for Purescript-Concur + Webpack

## Usage

### Get the purescript-concur dependency

> git submodule update --init --recursive

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
