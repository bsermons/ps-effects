"use strict";

// module Window

exports._openWindow = function (url, name, opts) {
    return function () {
        return window.open(url, name, opts);
    };
};

exports.closeWindow = function (winRef) {
    return function () {
        winRef.close();
        return {};
    };
};


exports._hasProperty = function (winRef, name) {
    return function (name) {
        return function() {
            return winRef.hasOwnProperty(name) && !!winRef[name];
        };
    };
};

exports._getProperty = function (winRef) {
    return function(name) {
        return function() {
            return winRef[name];
        };
    };
};
