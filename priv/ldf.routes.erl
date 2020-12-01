#{prefix => "",
  security => false,
  routes => [
            {"/receiver", { ldf_receiver_controller, message}, #{methods => [post,get]}},
            {"/li", {ldf_li_controller, manage_li}, #{methods => [post, get]}},
            {"/li/:liid", {ldf_li_controller, delete_li}, #{methods => [delete]}}
           ],
 statics => [
             {"/www/admin", "assets/admin.html"},
             {"/www/receiver", "assets/receiver.html"}
            ]
}.