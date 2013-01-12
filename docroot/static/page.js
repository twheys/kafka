
function addHTML(HTML) {
	var element = document.getElementById("html_box");
	element.appendChild(HTML);
	scrollToBottom("html_box");
}

function addText(Text) {
	var txt = document.createElement("div");
	txt.innerHTML = Text;
	document.getElementById("html_box").appendChild(txt);
	scrollToBottom("html_box");
}

function addSystemMsg(Msg) {
	addText('<span style="color:black;">*** ' + Msg + ' ***</span>');
}

function onTextEnter(e) {
	var characterCode;

	if(e && e.which) {
		e = e;
		characterCode = e.which;
	}		
	else{
		e = event;
		characterCode = e.keyCode;
	}
	
	else if(characterCode == 13) { 
		
		var target = document.getElementById("cur_target");
		var element = document.getElementById("chat_txt");
		
		if (target) {
			sendChatMessage(element.value, target.innerText);
		}
		else{
			sendChatMessage(element.value);
		}
		
		element.value = "";
		return false;
	}
	else{
		return true;
	}
}

function addToUsers(HTML) {
	var element = document.getElementById("users");
	element.appendChild(HTML);
}

function addUser(Nick) {
	var txt = document.createElement("div");
	txt.id = "cur_target";
	txt.style.width = "90%";
	txt.style.padding = "5px";
	txt.innerHTML = '<span style="border-bottom: 1px solid black; padding-top: 5px; padding-left: 5px; color: blue;">' + Nick.escapeHTML() + '</span>';
	addToUsers(txt);
}

function removeUser(Nick) {
	var u = document.getElementById("users_" + Nick.escapeHTML());
	document.getElementById("users").removeChild(u);
}

function updateMsgID(x) {
	if (x >= msgID) {
		msgID = x + 1;
	}
}

function setChatMsg(m) {
	document.getElementById("chat_txt").value = m;
	document.getElementById("chat_txt").focus();
}

function handleServiceMsg(response) {
    
	if(response.status == "ok") {
		if(response.response == "reconnect") return true;
		for(var x = 0; x < response.response.length; x++) {
			var data = response.response[x].d;
			var id = response.response[x].id;
			var t = response.response[x].t;
			
			updateMsgID(id);
			
			if(t == "user_joined_room") {
				addSystemMsg(data.escapeHTML() + " has joined the room.");
				addUser(data.escapeHTML());
			}
			else if(t == "user_left_room") {
				addSystemMsg(data.nick.escapeHTML() + " has left the room(" + data.reason.escapeHTML() + ")");
				removeUser(data.nick.escapeHTML());
			}
			else if(t == "chat_msg"){
				addText('<span style="color:red;">' + data.nick.escapeHTML() + "</span>: " + data.msg.escapeHTML());
			}
			else if(t == "sent_chat_msg"){
				addText('<span style="color:blue;">' + data.nick.escapeHTML() + "</span>: " + data.msg.escapeHTML());
			}
			else if(t == "admin_logged_in") {
			    addSystemMsg(data.escapeHTML() + " became an administrator.");
			}
			else if(t == "admin_logged_out") {
			    addSystemMsg(data.escapeHTML() + " gave up admin privileges.");
			}
			else if(t == "system_msg") {
			    addSystemMsg(data);
			}
		}
		return true;
	}
	else if(response.status == "error") {
	 	if(response.response == "bad_session") addSystemMsg("Your session is no longer valid. Please <a href=\"/\">login</a> again.");
		else addSystemMsg(response.response);
		return false;
	}
	else {
		addSystemMsg("Received an invalid response from the server. Please refresh this page.");
		return false;
	}
}