var msgID = 0;

function scrollToBottom(elm_id)
	{
	var elm = document.getElementById(elm_id);
	try
		{
		elm.scrollTop = elm.scrollHeight;
		}
	catch(e)
		{
		var f = document.createElement("input");
		if (f.setAttribute) f.setAttribute("type","text")
		if (elm.appendChild) elm.appendChild(f);
		f.style.width = "0px";
		f.style.height = "0px";
		if (f.focus) f.focus();
		if (elm.removeChild) elm.removeChild(f);
		}
	}

function onleave() {
    
}

function sendChatMessage(msg) {
	new Ajax.Request('/chat/send_msg/',
	  {
	    method:'post',
	    parameters: {msg: msg},
	    onSuccess: function(transport){
	    },
	    onFailure: function(){ }
	  });
}

function sendChatMessage(msg, target) {
	new Ajax.Request('/chat/send_priv_msg/',
	  {
	    method:'post',
	    parameters: {msg: msg},
	    parameters: {target: target},
	    onSuccess: function(transport){
	    },
	    onFailure: function(){ }
	  });
}

function logout() {
	new Ajax.Request('/chat/leave/',
	  {
	    method:'get',
	    onSuccess: function(transport){
	        window.location='/';
	    },
	    onFailure: function(){ }
	  });
}

function getServiceMsg() {
	new Ajax.Request('/chat/wait/?msg_id=' + msgID,
	  {
	    method:'get',
	    onSuccess: function(transport){
	        if(transport.responseText == "") {
	            setTimeout('getServiceMsg();', 10000);
	        }
	        else {
	            var response = transport.responseText.evalJSON();
	            if(handleServiceMsg(response) == true) {
		  	        getServiceMsg();
		        }
	        }
	    },
	    onFailure: function(){ 
	        setTimeout('getServiceMsg();', 10000) }
	  });
}

function getOnlineUsers() {
    new Ajax.Request('/chat/online/',
	  {
	    method:'get',
	    onSuccess: function(transport){
			var response = transport.responseText.evalJSON();
			
			if(response.status == "ok") {
				for(var x = 0; x < response.response.length; x++) addUser(response.response[x]);
				getServiceMsg();
			}
			else {
			    addSystemMsg("Unable to access user list at this time.");
			}
	    },
	    onFailure: function(){ addSystemMsg("The chat service is not responding at this time.") }
	  });
}

function startClient() {
	addSystemMsg("Establishing connection to chat service...");
	new Ajax.Request('/chat/start/',
	  {
	    method:'get',
	    onSuccess: function(transport){
			var response = transport.responseText.evalJSON();
			
			if(response.status == "ok") {
				msgID = response.response;
				addSystemMsg("Connected to chat service.");
				getOnlineUsers();
			}
			else if(response.status == "error" && response.response == "bad_session") addSystemMsg("Unable to locate your session. Please <a href=\"/\">login</a> again.");
	    },
	    onFailure: function(){ addSystemMsg("The chat service is not responding at this time.") }
	  });
}