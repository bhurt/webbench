const db = require("./db.js");
const app = require("express")();
const _ = require("lodash");

app.use(require("body-parser").json());

app.get("/rest/v1/users", function(req,res,next) {

  return db.one(
    'SELECT COUNT(*) AS "cnt" FROM userView'
  ).then(function(data) {
    return parseInt(data.cnt);
  }).then(function(cnt) {
    return res.status(200).json(cnt);
  }).catch(function(err) {
    return next(err);
  });

});

app.post("/rest/v1/users", function(req,res,next) {

  const limit = parseInt(req.query.limit);
  const offset = parseInt(req.query.offset);

  const items = _.join(_.map(req.body.items, function(item) {
    const sortBy = item.sortBy;
    if(!(/^\w+$/.test(sortBy))) {
      throw new Error("Field does not look like a field: " + sortBy);
    }
    const field = sortBy;
    const direction = item.sortDir == "Ascending" ? "ASC" : "DESC";
    return field + " " + direction;
  }));

  return db.many(
    'SELECT * FROM userView ORDER BY ' + items + ' LIMIT ' + limit + ' OFFSET ' + offset
  ).then(function(data) {
    return res.status(200).json(data);
  }).catch(function(err) {
    return next(err);
  });

});

app.use(function(err, req, res, next) {
  return res.status(err.status || 500).json(err);
});

app.listen(3000, function() {
  console.log("Node.js Express API now listening on 3000");
});
