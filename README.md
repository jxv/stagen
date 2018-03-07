# stagen

`stagen` is a command line static site generator.
No code is needed to be compiled in order to generate a site.
However, the ease comes at the cost of some customizability.

Pages are parsed from markdown and may include CSS and JS scripts.

If you're looking to create a no-frills blog using markdown and perhaps a few scripts,
this is it.

## Dates

The date of a post is parsed from the file's name.

It follows the format of:
```YYYY-MM-DD-rest-of-the-name.md```

## Title

The title shown for each page is parsed from the very first line of the file.

```markdown
# The Title

The content.
```

The text `The Title` will be placed within the `<title>` tag inside the generated html file.

## Archive

The archive is (often) the home page which lists dated posts in chronological order.

## Feeds

The feeds are automatically generated as atom - `atom.xml`, json feed - `feed.json` and rss - `rss.xml`.
As expected, only dated posts are inside the feeds.

## Installation

[Download stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)

```shell
stack build
stack install
```

### Quick test if installed

Try

```shell
stagen
```

Should output this

```shell
Missing: COMMAND

Usage: stagen COMMAND
```

## Example

Check out the [example](https://github.com/jxv/stagen/tree/master/example) blog.
