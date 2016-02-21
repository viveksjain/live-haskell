var express = require('express');
var bodyParser = require('body-parser');
var child_process = require('child_process');
var fs = require('fs');
var app = express();

app.use(express.static('../client'));
app.post('/evaluate', bodyParser.urlencoded({extended: false}), function (req, res) {
  if (!req.body || !req.body.script) return res.sendStatus(400)
  console.log(req.body);
  fs.writeFile('/tmp/test.hs', req.body.script, function (err) {
    if (err) {
      console.error('Writing error');
      console.error(err);
      res.sendStatus(500)
    } else {
      child_process.exec('runhaskell /tmp/test.hs', function (err, stdout, stderr) {
        if (err) {
          console.error('Execution error');
          console.error(err);
          res.sendStatus(500)
        } else if (stderr) {
          res.send({error: stderr})
        } else {
          res.send({output: stdout})
        }
      });
    }
  });
});

app.listen(3000, function () {
  console.log('Listening on port 3000');
});
