var fs = require('fs');
var path = require('path');
var program = require('commander');
var mkdirp = require('mkdirp');
var async = require('async');


program
    .version('0.0.0')
    .option('-c, --compile <path>', 'where less lives')
    .option('-o, --outcome <path>', 'where stylus goes');

program.parse(process.argv);

var sourcePath = program.compile;
var targetPath = program.outcome;

// helper functions
var isLESS = function (path) {
    return /\.less$/.test(path);
};

var transpile = function (content) {
    return content;
};

var getDirectoriesAndPathsReady = function (sourcePath, targetPath, callback) {
    fs.exists(sourcePath, function (exists) {       // source path
        if (exists) {    
            mkdirp(targetPath, function (err) {     // target path, mkdir if not exist
                if (err) throw err;
                fs.readdir(sourcePath, function (err, files) {  // readdir
                    if (err) throw err;
                    // filter & join
                    callback(files.filter(isLESS).map(function (filepath) {
                        return path.join(sourcePath, filepath);
                    }));
                })
            });
        }
    });
};


getDirectoriesAndPathsReady(sourcePath, targetPath, function (paths) {

    async.forEach(paths, function (filepath, callback) {
        fs.readFile(filepath, function (err, content) {
            if (err) throw err;
            transpiledContent = transpile(content);
            fs.writeFile(filepath.replace(/less$/, 'styl').replace(sourcePath, targetPath), transpiledContent, callback);
        });
    }, function (err) {
        if (err) throw err;
        console.log('all done');
    })
});