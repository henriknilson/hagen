
# Hagen
## About
We want to create a simple static site generator, that can parse templates and text files. It should output a single HTML file while
reading data from yaml- or txt-files, and then writing that data to a HTML file. The idea is presented below.

## Todo
- [x] Parse mustache tags
- [x] Load data from files
- [x] Parse directories
- [x] Add #include syntax for partials
- [ ] Add #for syntax
- [ ] Copy all assets to public/

## Project Structure
```
src/
	assets/
		style.css
	pages/
		2016-11-29.txt
		2016-11-30.txt
		about.txt
	template/
		index.html
		about.html
		single.html
	partials/
		header.html
		footer.html
		post-thumbnail.html
```
## Example files
index.html
```
<html>
{{ #include header }}
{{ #for post in posts }}
<h2>{{ $title }}</h2>
<a href="{{ $link }}">Read more</a>
{{ #endfor }}
{{ #include footer }}
</html>
```

single.html
```
{{ #include header }}
<h2>{{ $title }}</h2>
<p>{{ $text }}</p>
<p>{{ $date }}</p>
{{ #include footer }}
```
2016-11-29.txt
```
template: single
type: blogpost
title: Good day!
text: bkdjsdlsdndfkefkjfd
date: 2016 11 29
```

## Output
```
public/
	index.html
	style.css
	script.js
```
### Built by
Henrik Nilson && Filip Hallqvist
