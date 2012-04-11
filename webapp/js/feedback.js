// Source: http://www.alexpeattie.com/projects/feedback_button/

var feedback_btn = document.getElementById("feedback");
var feedback_link = feedback_btn.getElementsByTagName("a")[0];

if(feedback_btn.style.Transform == undefined && feedback_btn.style.WebkitTransform == undefined && feedback_btn.style.MozTransform == undefined && feedback_btn.style.OTransform == undefined && feedback_btn.filters == undefined) {

    feedback_link.style.width="15px";
    feedback_link.style.height="70px";
    feedback_link.style.padding="16px 8px";
    feedback_link.innerHTML = "<object type='../img/svg+xml' data='feedback_vertical.svg'></object>";

}
