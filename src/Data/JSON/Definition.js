"use strict";

exports.writeYaml = function (value) {
  const yaml = require("js-yaml")
  return yaml.dump(value)
}
