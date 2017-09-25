var bower = require('bower');

exports._getLatest = function (packageName) {
  return function (err) {
    return function (succ) {
      return function () {
        bower.commands.info(packageName)
          .on('end', function (results) {
            succ(results)();
          })
          .on('error', function (e) {
            err(e)();
          });
      };
    };
  };
};
