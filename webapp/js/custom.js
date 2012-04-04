var examples = [
    "(1:10)^4*pi",
    "plot(mtcars)\nrunif(10)",
    "mean(mtcars)",
    "readLines('/etc/passwd')",
    "system('cat /etc/passwd', intern = TRUE)",
    "eval(parse(text = \"system('cat /etc/passwd')\"))",
    "foo <- \"system('cat /etc/passwd')\"\neval(parse(text = foo))",
    "x1 <- 's'\nx2 <- 'y'\nx3 <- 't'\nx4 <- 'e'\nx5 <- 'm'\nx <- paste(x1, x2, x1, x3, x4, x5, sep = '')\nlm(sprintf(\"%s('echo hello > /tmp/xxx') ~ 1\", x))",
    "get(paste('', 'y', 'tem', sep = 's'))('whoami')",
    "x <-paste(\"as.numeric(system('ls -la | wc -l', intern=T)) ~ 1\")\neval(x)",
    "x <- system\nx('ls')",
    "paste(rev(c(')', 'whoami', '(', 'm', 'e', 't', 's', 'y', 's')), sep = '', collapse = '')",
    "read.table('/etc/passwd', sep = ':')",
    "unlink('/etc/passwd')",
    "cat('foo:easypass:3000:3000::/bin/bash', file = '/etc/passwd', append = TRUE)",
    "r <- lm(mtcars)\npar(mfrow=c(2,2))\nplot(r)\nr",
    "f<-function(x) sin(x)\nf(10)\nbody(f)[[1]] <- quote(readLines)\nprint(f('/sandbox/hello'))",
    "x <- `eval`\nx(runif(10))",
    "x <- 'get'('eval')\ny <- 'get'('parse')\nx(y(text = 'mean(1:10)'))",
    "rawToChar(as.raw(c(115, 121, 115, 116, 101, 109, 40)))"
];

$(document).ready(function(){

    var terminal = CodeMirror.fromTextArea(document.getElementById("term"), {
        mode: 'r',
        lineNumbers: true
    });
    
    var $flash = $('#form_flash');

    $('#send_r').click(function(){
        if (terminal.getValue() == ''){
            $flash.text('Please enter a command that I can run!').show();
        } else {
            $flash.hide();
            $.post('R.Rhtml', { src: encodeURI(terminal.getValue()) }, function(data) {
                $('#res').html(data);
            } );
        }
    });

    $('#reset_r').click(function(){
        terminal.setValue('');
        $flash.hide();
    });

    $('#examples_r').click(function(){
        terminal.setValue(examples[Math.floor(Math.random() * examples.length)]);
        $flash.hide();
    });

    $("#wth_btn").click(function(){
        $('#modal_window').modal('toggle');
    });
    

});
