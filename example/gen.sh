#! /bin/sh

stagen clean -e header.md -f footer.md -a index.md -c style.css -i README.md -u http://localhost -t Blog
stagen build -e header.md -f footer.md -a index.md -c style.css -i README.md -u http://localhost -t Blog
