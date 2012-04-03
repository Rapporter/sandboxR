function send2R() {
    $.post('R.Rhtml', { src: encodeURI(terminal.getValue()) }, function(data) {
          $('#res').html(data);
      } );
};

var examples=new Array();
examples[0]="(1:10)^4*pi";
examples[1]="plot(mtcars)\nrunif(10)";
examples[2]="mean(mtcars)";
examples[3]="readLines('/etc/passwd')";
examples[4]="system('cat /etc/passwd', intern = TRUE)";
examples[5]="eval(parse(text = \"system('cat /etc/passwd')\"))";
examples[6]="foo <- \"system('cat /etc/passwd')\"\neval(parse(text = foo))";
examples[7]="x1 <- 's'\nx2 <- 'y'\nx3 <- 't'\nx4 <- 'e'\nx5 <- 'm'\nx <- paste(x1, x2, x1, x3, x4, x5, sep = '')\nlm(sprintf(\"%s('echo hello > /tmp/xxx') ~ 1\", x))";
examples[8]="get(paste('', 'y', 'tem', sep = 's'))('whoami')";
examples[9]="x <-paste(\"as.numeric(system('ls -la | wc -l', intern=T)) ~ 1\")\neval(x)";
examples[10]="x <- system\nx('ls')";
examples[11]="paste(rev(c(')', 'whoami', '(', 'm', 'e', 't', 's', 'y', 's')), sep = '', collapse = '')";
examples[12]="read.table('/etc/passwd', sep = ':')";
examples[13]="unlink('/etc/passwd')";
examples[14]="cat('foo:easypass:3000:3000::/bin/bash', file = '/etc/passwd', append = TRUE)";
examples[15]="r <- lm(mtcars)\npar(mfrow=c(2,2))\nplot(r)\nr"

