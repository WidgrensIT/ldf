#{prefix => "",
  security => false,
  routes => [
            {"/receiver", { ldf_receiver_controller, message}, #{methods => [post,get]}},
            {"/message/:messageid", { ldf_message_controller, message}, #{methods => [get]}},
            {"/li", {ldf_li_controller, manage_li}, #{methods => [post, get]}},
            {"/history", {ldf_li_controller, manage_history}, #{methods => [post]}},
            {"/li/:liid", {ldf_li_controller, delete_li}, #{methods => [delete]}}
           ],
 statics => [
             {"/www/admin", "assets/admin.html"},
             {"/www/receiver", "assets/receiver.html"},
             {"/assets/[...]", "assets"}
            ]
}.