"use strict";

const yaml = require("js-yaml")

exports.writeYaml = function (value) {
  return yaml.dump(value)
}
