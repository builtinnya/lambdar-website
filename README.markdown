# Lambdar

> Lambdar is still in early development stage and missing some features.

This is the repository of my personal website, [Lambdar][].
Lambdar is developed using [Yesod][], a web framework for [Haskell][].

[Lambdar]: http://lambdar.me/
[Yesod]: http://www.yesodweb.com/
[Haskell]: https://www.haskell.org/

## Features

- Deadly simple blog app for me
- Support multi-language articles
- Support [GitHub-Flavored Markdown][] to write articles
- Support command-line management tools to publish articles

[GitHub-Flavored Markdown]: https://help.github.com/articles/github-flavored-markdown/

## Development

To develop Lambdar, you need to install [GHC][] and [Yesod][] on your machine.
The following table shows which version of software you need.

[GHC]: https://www.haskell.org/ghc/

| Name | Version |             Note |
|------|---------|-----------------:|
| GHC  | 7.8.4   | Haskell compiler |

You can read [a great book for Yesod](http://www.yesodweb.com/book) on its website.
The book helps you a lot while developing a web application.
[Yesod wiki](https://github.com/yesodweb/yesod/wiki) is also a useful resource.


### Setup on Mac OS X

To install [GHC][] on Mac OS X, you can use [Homebrew][]:

```bash
$ brew update
$ brew install ghc cabal-install
```

[Homebrew]: http://brew.sh/

To install [Yesod][] binaries, use [Cabal][]:
```bash
$ # You may change the following line depending on your shell setup
$ echo 'export PATH=$HOME/.cabal/bin:$PATH' >> ~/.profile
$ cabal update
$ cabal install cabal-install
$ cabal install yesod-bin
```

[Cabal]: https://www.haskell.org/cabal/

## Server Setup

Lambdar is deployed using [Keter][], a web application deployment manager.
Currently, Lambdar is only tested on [Ubuntu 14.04.2 LTS][].

[Keter]: https://github.com/snoyberg/keter
[Ubuntu 14.04.2 LTS]: http://releases.ubuntu.com/14.04/

### Setup on Ubuntu 14.04.2 LTS

To install [Keter][], follow [the instructions on the repo](https://github.com/snoyberg/keter#setup):
```bash
$ sudo apt-get install haskell-platform
$ cabal update
$ cabal install keter
$ sudo mkdir -p /opt/keter/bin
$ sudo cp ~/.cabal/bin/keter /opt/keter/bin
$ sudo mkdir -p /opt/keter/incoming
$ sudo chown $USER /opt/keter/incoming
```

You also need to follow the Lambdar-specific setup:
```bash
$ # To ensure Sqlite database file is persistent through deployment
$ sudo mkdir -p /opt/keter/database
$ # Directory for scripts needed to manage production database
$ sudo mkdir -p /opt/keter/scripts
$ sudo chown $USER /opt/keter/scripts
```

Create a Keter config file `/opt/keter/etc/keter-config.yaml` ([sample](https://github.com/snoyberg/keter/blob/master/etc/keter-config.yaml)):
```yaml
root: ..
listeners:
    - host: "*4" # Listen on all IPv4 hosts
```

You may want to set up an Upstart job for `keter`:
```
# /etc/init/keter.conf
start on (net-device-up and local-filesystems and runlevel [2345])
stop on runlevel [016]
respawn

console none

exec /opt/keter/bin/keter /opt/keter/etc/keter-config.yaml
```

That's it.
You can run Keter by:
```bash
$ sudo start keter
```

## Compilation & Deployment

We don't want to compile a web app on server as [mentioned in Yesod book](http://www.yesodweb.com/book/deploying-your-webapp#deploying-your-webapp_compiling).
However, my development machine runs Mac OS X while the server machine runs Ubuntu.
Lambdar uses [Docker][] to avoid this problem by compiling in a Ubuntu-based container.

[Docker]: https://www.docker.com/

The first step is to create a Docker image:
```bash
$ cd /path/to/lambdar/repo
$ docker build -t builtinnya/lambdar .
```

Then, every time you want to deploy Lambdar, all you need to do is:
```bash
$ cd /path/to/lambdar/repo
$ ./deploy
```

**NOTE: You need to set up Keter on server and SSH config for 'lambdar' host**

Local SSH config `~/.ssh/config` will be something like this:

```
Host lambdar
     User nya
     IdentityFile ~/.ssh/id_rsa
     HostName lambdar.me
     Port 443
```

## Management

Articles are written in [GitHub-Flavored Markdown][] and converted to HTML when
registering to database.
Lambdar provides command-line management tools to create, read, update, and delete
articles, languages, and tags.

Management scripts (e.g. `app/article.hs`) are compiled into binaries (e.g. `dist/build/article/article`) and deployed to server.
You have to invoke these binaries on server to access production database.
This is what `manage` script takes care of.

### Languages

To list all currently available languages:
```bash
$ ./manage lang list
```

To add a new language:
```bash
$ ./manage lang add SLUG NAME
```

`SLUG` becomes a part of URL. `NAME` is a displayed name.

To update a language's displayed name:
```bash
$ ./manage lang update SLUG NAME
```

To delete a language:
```bash
$ ./manage lang delete SLUG
```

### Tags

To list all currently available tags:
```bash
$ ./manage tag list
```

To add a new tag:
```bash
$ ./manage tag add SLUG NAME [DESCRIPTION]
```

`SLUG` becomes a part of URL. `NAME` is a displayed name.
`DESCRIPTION` is optional.

To update a tag's name and description:
```bash
$ ./manage tag update SLUG [--name NAME] [--description DESCRIPTION]
```

To delete a tag:
```bash
$ ./manage tag delete SLUG
```

### Articles

To list all currently registered articles:
```bash
$ ./manage article list
```

To add a new article:
```bash
$ ./manage article add LANG SLUG TITLE SOURCEFILE
```

`LANG` is the language slug of the article.
`SLUG` becomes a part of URL.
`TITLE` is the title of the article.
`SOURCEFILE` is relative path to a markdown file.

To update article's meta data:
```bash
$ ./manage article update LANG SLUG [--title TITLE]
```

To attach tags to an article:
```bash
$ ./manage article tag SLUG [--tags TAGS]
```

`TAGS` is a comma-separated list of tag slugs to be attached to the article.

To update article's content:
```bash
$ ./manage article content-update LANG SLUG [--sourcefile SOURCEFILE]
```

## TODOs

- [ ] Automate database backup
- [ ] Implement search
- [ ] Implement RSS feed

## License

Copyright © 2015-present Naoto Yokoyama

Distributed under the MIT license. See the [LICENSE](./LICENSE) file for full details.
