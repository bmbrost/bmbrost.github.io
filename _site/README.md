# Indigo2

This is an attempt by to pare down Indigo, "a simple and minimalist template for Jekyll for those who likes to eat noodles".
Indigo was created by Sérgio Kopplin under the MIT license.
Indigo2 is a version of Indigo modified by Jimmy O'Donnell under the same license.


## Instructions

Note there is a [FAQ](./FAQ.md).

0. Edit `_config.yml` with your data (check <a href="README.md#settings">settings</a> section)

```
name: John Doe
bio: 'A Man who travels the world eating noodles'
picture: 'assets/images/profile.jpg'
...
and lot of other options, like width, projects, pages, read-time, tags, related posts, animations, multiple-authors, etc.
```

To build and test locally:

1. Install [Jekyll](http://jekyllrb.com), [NodeJS](https://nodejs.org/) and [Bundler](http://bundler.io/).
2. Clone the forked repo on your machine
3. Enter the cloned folder via terminal and run `bundle install`
4. Then run `bundle exec jekyll serve --config _config.yml,_config-dev.yml`
5. Open it in your browser: `http://localhost:4000`
6. Test your app with `bundle exec htmlproofer ./_site`
7. To edit the posts using the [jekyll-admin](https://jekyll.github.io/jekyll-admin/) plugin, go to the admin panel: `http://localhost:4000/admin`. The admin panel will not work on GitHub Pages, [only locally](https://github.com/jekyll/jekyll-admin/issues/341#issuecomment-292739469).


## Contents

- [Jekyll](https://jekyllrb.com/), [Sass](http://sass-lang.com/) ~[RSCSS](http://rscss.io/)~ and [SVG](https://www.w3.org/Graphics/SVG/)
- Tests with [Travis](https://travis-ci.org/)
- Google Speed: [98/100](https://developers.google.com/speed/pagespeed/insights/?url=http%3A%2F%2Fsergiokopplin.github.io%2Findigo%2F);



---

[MIT](http://kopplin.mit-license.org/) License © Sérgio Kopplin
