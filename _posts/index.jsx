var async = require('asyncawait/async');
var await = require('asyncawait/await');
var Promise = require('bluebird');
var fs = Promise.promisifyAll(require('fs')); // adds Async() versions that return promises
var path = require('path');
var _ = require('lodash');

/** Returns the number of files in the given directory. */
var countFiles = async (function (dir) {
    var files = await (fs.readdirAsync(dir));
    var paths = _.map(files, function (file) { return path.join(dir, file); });
    var stats = await (_.map(paths, function (path) { return fs.statAsync(path); })); // parallel!
    return _.filter(stats, function (stat) { return stat.isFile(); }).length;
});

// Give it a spin
countFiles(__dirname)
    .then (function (num) { console.log('There are ' + num + ' files in ' + __dirname); })
    .catch(function (err) { console.log('Something went wrong: ' + err); });