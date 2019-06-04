remark.macros.scale = function (percentage) {
  var url = this;
  return '<img src="' + url + '" style="width: ' + percentage + '" />';
};

remark.macros.vspace = function (percentage) {
  return `<div style="height: ${percentage}%"></div>`;
};
