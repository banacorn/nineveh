
love:
	node index.js -c test/less -o test/stylus
	stylus --out test/css/bootstrap.css test/stylus/bootstrap.stylus