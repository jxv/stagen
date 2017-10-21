#! /bin/sh

stagen clean -e header.md -f footer.md -a index.md -c style.css -i README.md
stagen build -e header.md -f footer.md -a index.md -c style.css -i README.md
