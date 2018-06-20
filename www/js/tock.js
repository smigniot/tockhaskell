//
// TODOLIST
//
// === ORIGINAL ===
// [X] Refresh sur erreur de traffic réseau
// [X] Dernieres cartes jouees toujours affichees
// [X] Permettre de cacher l'erreur
// [X] Montrer quand un echange a eu lieu, visual feedback
// [X] Montrer qui joue, notamment soi/pas soi
// [X] Supprimer les cartes precedemment montrees
// [X] Cercles colorés pour les bases
// [X] Couronne pour les 8 (1/4 du rayon)
// [X] Parkings en police noire
// [X] Parkings avec marge 1/3 de diamètre
// [X] Pointillés aux 8
// [X] Noms de joueurs horizontaux
// [X] Centre avec triangles de couleur : carré comme jeu original
// [X] Refaire l'interaction pour participer a la partie
// [X] MyOwn transitions - too bad for requestAnimationFrame
// [X] Animation sur victoire
// === BUGS ===
// [X] Dessiner la carte avant l'anim
// [ ] Animation en entrée de pion
// [ ] Animation sur sept
// [ ] Animation sur swap
// [ ] Feedback sur clic de carte
// [ ] Supprimer les listeners sur carte jouée
// [ ] Pawn size on mobile, how about minimap ?
// [ ] Supprimer vite les pions pris
// [ ] Dessiner la carte donnee avant l'anim
// [ ] Donne amene jusqu'au triangle
// [ ] Moins d'ecart dans les cartes jouees
// === IMPROVEMENTS ===
// [ ] Top-left : qui joue, quoi faire, etat en cours
// [ ] Echange fais glisser la carte vers / On peut la poser vers
// [ ] Never the debug-thingamabob « Select » for moves
// [ ] Single clic on pawn when ambiguous a plus
// [ ] Proposer des noms par défaut, permettre le changement
// [ ] Dogfree is a minimum comparison
//
var pings = [];
var MAXPING = 10;
function inform(message, error) {
    if(error) {
        console.log(message, error);
        d3.select("header.information")
            .style("display","")
            .text(message)
            .append("button")
            .text("Refresh?")
            .style("margin-left", "50px")
            .style("font-size", "0.8em")
            .style("padding","5px")
            .on("click", function() {
                document.location.reload();
            })
            ;
        throw error;
    } else {
        pings.push(new Date());
        if(pings.length > MAXPING) {
            pings.splice(0,1);
        }
    }
}

function boot() {
    function reload() {
        updateGames(function() {
            setTimeout(reload, 3000);
        });
    } 
    d3.select("nav.searchgames input").node().focus();
    reload();
}

var frdir = {
    west: "Ouest",
    east: "Est",
    north:"Nord",
    south:"Sud"
};
function updateGames(finished) {
    d3.csv("games.csv", function(err, games) {
        inform("Can't load games.csv", err);
        
        games.reverse();
        var sel = d3.select("table.games").selectAll("tr.game").data(games);
        var tr = sel.enter().append("tr").attr("class","game");
        tr.append("td");
        tr.append("td");
        var lasttd = tr.append("td");
        "west,north,east,south".split(/,/).forEach(function(direction) {
            var upper = direction.slice(0,1).toUpperCase()+direction.slice(1);
            var sp = lasttd
                .append("div").attr("class","extbtright")
                .append("span").attr("class","buttonright");
            sp.append("input").attr("type","text")
                .attr("class",direction)
                .attr("placeholder",frdir[direction])
                .attr("onkeyup", "if(event.keyCode == 13) this.nextSibling.click()")
                ;
            sp.append("button").html("&crarr;")
                .attr("onclick", "joinGame(this.previousSibling,'"+upper+"')")
                ;
        });
        sel.exit().remove();

        function updateInput(tr, selector, value) {
            var n = tr.select(selector).node();
            if(n && (n != document.activeElement)) {
                if(n.value != value) {
                    n.value = value;
                }
            };
        }
        d3.select("table.games").selectAll("tr.game").each(function(game) {
            tr = d3.select(this);
            tr.select("td").text(game.name);
            var when = moment(game.creation+"Z").fromNow();
            tr.select("td:nth-child(2)").text(when);
            updateInput(tr, "input.west", game.west);
            updateInput(tr, "input.north", game.north);
            updateInput(tr, "input.east", game.east);
            updateInput(tr, "input.south", game.south);
        });
        if(finished) finished();
    });
}

function searchGame(input) {
    var pattern = input.value.trim();
    console.log("Search or create", pattern);
    var table = d3.select("table.games");
    table.selectAll("tr.game").classed('hidden', function(d) {
        return !(d.name.match(pattern));
    });

    if(pattern) {
        var a = table.selectAll("tr.game.hidden").size();
        var b = table.selectAll("tr.game").size();
        if(a == b) {
            d3.request("games")
                .header("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8")
                .post("gamename="+encodeURIComponent(pattern), function(err,resp) {
                    inform("Failed to create game", err);
                    table.selectAll("tr.game").classed("hidden", false);
                    updateGames();
                });
        }
    }
}

function joinGame(input, direction) {
    var playername = input.value.trim();
    var game = d3.select(input).datum();

    if(playername) {
        var uri = "game/"+game.creation+"/"+encodeURIComponent(game.name);
        d3.request(uri+"/join")
            .header("Content-Type", "application/x-www-form-urlencoded;charset=utf-8")
            .post("position="+direction+"&"+direction+"="+encodeURIComponent(playername), 
            function(err,resp) {
                inform("Failed to join game", err);
                window.location.href = uri+"/"+direction+"/play.html";
            });
    }
}

function bootplay() {
    var pingview = d3.select("body").append("div")
        .style("position","fixed")
        .style("top",0)
        ;

    d3.request("../../../../img/material.svg")
        .get(function(err,xhr) {
            inform("Can't load tock board image", err);
            d3.select("main section").html(xhr.response);

            createInfoBins();
            allowPawnDrag();
            updateStatus(continuousUpdateHistory);
        });
}

function allowPawnDrag() {
    var allPawnIds = []
    "west,north,east,south".split(/,/).forEach(function(direction) {
        [0,1,2,3].forEach(function(j) {
            allPawnIds.push("#pawn"+direction+j);
        });
    });
    var drag = d3.drag()
        .on("drag", function(d) { 
            var x = d3.event.x, y=d3.event.y,
                cells = [].slice.call(document.querySelectorAll("g.tockcell")),
                distances = cells.map(function(cell,n) {
                    var bb = cell.getBBox(),
                        cx = bb.x+bb.width/2,
                        cy = bb.y+bb.height/2,
                        dx = cx-x, dy=cy-y,
                        dist = Math.sqrt(dx*dx+dy*dy)
                        ;
                    return { dist:dist, cell:cell, n:n, x:cx, y:cy }
                })
                ;
            var best = distances[0];
            distances.slice(1).forEach(function(d) {
                if(d.dist < best.dist) best = d;
            });
            if(best.dist < 24) {
                x = best.x;
                y = best.y;
                var d = d3.select(this).datum();
                d.userMovedTo = best.n;
            } else {
                d.userMovedTo = "Out";
            }
            d3.select(this)
                .attr("cx", x)
                .attr("cy", y)
                ;
        })
        ;
    d3.selectAll(allPawnIds.join(",")).call(drag);
}

var OLD_colordeltas = {
    west:  {base:24, dx:0,  dy:-1, ta:"end",   r:90},
    south: {base:0,  dx:-1, dy:0,  ta:"end",   r:0},
    north: {base:48, dx:1,  dy:0,  ta:"start", r:0},
    east:  {base:72, dx:0,  dy:1,  ta:"start",   r:90},
};
var colordeltas = {
    west:  {base:24, dx:0, dy:-1, ta:"end", r:90, ndy: -1, ndx:4, 
        partner:"east"},
    south: {base:0,  dx:-1, dy:0,  ta:"end",   r:0, partner:"north"},
    north: {base:48, dx:1,  dy:0,  ta:"start", r:0, partner:"south"},
    east:  {base:72, dx:0,  dy:1,  ta:"start",   r:90, ndy: -1,
        partner:"west"}
};
function createInfoBins() {
    var g = d3.select("svg").append("g");
    var g2 = d3.select("svg").append("g").attr("class","exchangedicons");
    var cells = [].slice.call(document.querySelectorAll("g.tockcell"));
    "west,north,east,south".split(/,/).forEach(function(direction,j) {
        var d = colordeltas[direction];
        var cell = cells[d.base];
        var bb = cell.getBBox();
        var cx = bb.x+bb.width/2;
        var cy = bb.y+bb.height/2;
        var x = cx+(1.5*d.dx)*42;
        var y = cy+(1.5*d.dy)*42;
        x += -32*d.dy;
        y += 32*d.dx;
        if(d.ndy) { y += 16*d.ndy*d.dy; }
        if(d.ndx) { x+=d.ndx; }
        var gt = g.append("g").attr("transform", "translate("+x+","+y+")");
        gt.append("text")
            .text("...")
            .attr("class", "playername "+direction)
            //.attr("transform", "rotate("+d.r+")")
            .style("text-anchor", d.ta)
            ;

        var back = document.querySelector("#BACK_BLUE1").cloneNode(true);
        back.removeAttribute("id");
        back.style.display = "block";
        back.setAttribute("transform","");
        back.setAttribute("transform","");
        g2.node().appendChild(back);
        var bb2 = back.getBBox();
        var x = cx+(1.5*d.dx)*42;
        var y = cy+(1.5*d.dy)*42;
        x += -50*d.dy;
        y += 50*d.dx;
        if(direction == "west") { y -= 80; }
        if(direction == "south") { x -= 55; y -= 80; }
        if(direction == "east") { x -= 55;  }
        back.setAttribute("transform", [
            "translate(",x," ",y,") ",
            "scale(0.25) ",
            "translate(",-bb2.x," ",-bb2.y,")",
            ].join(""));
        d3.select(back)
            .attr("class", "exchange "+direction)
            .style("display", "none")
            ;
    });
}

function continuousUpdateHistory() {
    function reload() {
        updateHistory(function() {
            setTimeout(reload, 2000);
        });
    }
    reload();
}
function updateHistory(callback) {
    var game = d3.select("main").datum();
    var historyCount = (game && game.historyCount) || 0;
    var uri = "history/"+historyCount+"?freshen="+(+(new Date()));
    console.log("REQ", "history", new Date().toISOString()
        .replace(/^.*T([^\.]+).*$/,"$1"));
    d3.request(uri).get(function(err,xhr) {
        inform("Can't query game "+uri, err);
        var changeset = JSON.parse(xhr.response);
        if(changeset && changeset.length) {
            transit(changeset, function() {
                updateStatus(callback);
            }); // may change game.historyCount
        } else {
            callback();
        }
    });
}

function transit(moves, callback) {
    if(moves && moves.length) {
        var move = moves[0];
        applyMove(move, function() {
            transit(moves.slice(1), callback);
        });
    } else {
        callback();
    }
}

function pathto(a,b,rewind) {
    function ispark(n) { return (18<(n%24)) && ((n%24)<23); }
    var path = [];
    if(!rewind) {
        if(b<a) b+=96;
        var bparked = ispark(b);
        for(var i = a; i<=b; i++) {
            var n = i%96;
            if((!ispark(n)) || bparked) {
                path.push(n);
            }
        }
    } else {
        var i=a;
        while((i%96)!=(b%96)) {
            var n = i%96;
            if(!ispark(i))
            path.push(i);
            i--;
        }
        path.push(b);
    }
    return path;
}

function applyMove(diff, callback) {
    var game = d3.select("main").datum();

    game.lastfour.push(diff);
    updateLastFour(game);

    if((diff.type == "Move") && (diff.move.type == "Run")) {
        var path = pathto(diff.move.from.position, diff.move.to.position, 
            (diff.move.card.cardValue == 'Four'));
        var pawn = d3.selectAll(".pawn").filter(function(d) {
            return d && ((d.type == diff.move.from.type) &&
                    (d.position == diff.move.from.position));
        });
        pawn = d3.select(pawn.node());
        animateSinglePawn(pawn, path, 200, function() {
            game.historyCount++;
            callback();
        });
    } else if((diff.type == "Move") && (diff.move.type == "Swap")) {
        var a = d3.selectAll(".pawn").filter(function(d) {
            return d && ((d.type == diff.move.source.type) &&
                    (d.position == diff.move.source.position));
        });
        var b = d3.selectAll(".pawn").filter(function(d) {
            return d && ((d.type == diff.move.target.type) &&
                    (d.position == diff.move.target.position));
        });
        animateTwoPawns(a, diff.move.source.position, b, diff.move.target.position, function() {
            game.historyCount++;
            callback();
        });
    } else if((diff.type == "Move") && 
            (diff.move.type == "Enter")) {
        // no animation
        setTimeout(function() {
            // TODO: make a pawn enter
            console.log("DBG", "enter", diff);
            game.historyCount++;
            callback();
        }, 1000);
    } else if((diff.type == "Move") && 
            (diff.move.type == "Withdraw")) {
        // no animation
        setTimeout(function() {
            game.historyCount++;
            callback();
        }, 1000);
    } else if((diff.type == "Registration") ||
              (diff.type == "Exchanging") ||
              (diff.type == "Exchange") ||
              (diff.type == "Playing")) {
        // no animation
        game.historyCount++;
        callback();
    } else if((diff.type == "Move") && 
            (diff.move.type == "Seven")) {
        var hops = diff.move.hops;
        console.log("TODO: Seven", diff, hops);
        setTimeout(function() {
            game.historyCount++;
            callback();
        }, 1000);
    } else {
        console.log("TODO", diff);
        game.historyCount++;
        callback();
    }
}

function animateSinglePawn(pawn, path, delay, callback) {
    var cells = [].slice.call(document.querySelectorAll("g.tockcell"));
    var cell = cells[path[0]];
    var bb = cell.getBBox();
    var x = bb.x+bb.width/2;
    var y = bb.y+bb.height/2;

    function trail(p) {
        if(p.length) {
            var cell = cells[p[0]];
            var bb = cell.getBBox();
            var x = bb.x+bb.width/2;
            var y = bb.y+bb.height/2;
            pawn.attr("cx", x)
                .attr("cy", y)
                ;
            setTimeout(function(){trail(p.slice(1))}, delay);
        } else {
            callback();
        }
    }
    console.log("ANIM", "trail", new Date().toISOString()
        .replace(/^.*T([^\.]+).*$/,"$1"));;
    trail(path);
}

function animateTwoPawns(a,ca, b,cb, callback) {
    var cells = [].slice.call(document.querySelectorAll("g.tockcell"));
    var bba = cells[ca].getBBox();
    var bbb = cells[cb].getBBox();

    console.log("ANIM", "two", a,b, new Date().toISOString()
        .replace(/^.*T([^\.]+).*$/,"$1"));
    d3.selectAll("*").interrupt();
    var ta = a.transition()
        .duration(600)
        .attr("cx", bbb.x+bbb.width/2)
        .attr("cy", bbb.y+bbb.height/2)
        ;
    var tb = b.transition()
        .duration(600)
        .attr("cx", bba.x+bba.width/2)
        .attr("cy", bba.y+bba.height/2)
        ;
    tb.on("end", function() {
        console.log("ANIM", "finishedtwo", new Date().toISOString()
            .replace(/^.*T([^\.]+).*$/,"$1"));;
        callback();
    })
        ;
}
        
function updateStatus(finished) {
    console.log("REQ", "status", new Date().toISOString()
        .replace(/^.*T([^\.]+).*$/,"$1"));
    d3.request("status.json?freshen="+(+(new Date()))).get(function(err,xhr) {
        inform("Can't query game status.json", err);

        var game = JSON.parse(xhr.response);
        var l = location.pathname.split(/\//g),
            mycolor = l[l.length-2],
            me = {"color":mycolor}
            ;
        updateNames(game,me);
        updatePawns(game);
        updateHand(game,me);
        d3.select("main").data([game]);
        updateWon(game);
        if(finished) finished();
    });
}

function updateNames(game,me) {
    var exchanging = ((game.pc.type == "Exchanging") || (game.pc.type == "Exchange"));
    d3.selectAll("text.playername").style("font-weight","normal");
    d3.selectAll("text.playername").style("fill",null);
    if(exchanging) {
        d3.selectAll("text.playername").style("font-weight","bold");
    }

    "west,north,east,south".split(/,/).forEach(function(direction) {
        var upper = direction.slice(0,1).toUpperCase()+direction.slice(1);
        var player = game.players[upper];
        var name = player?player.name:"";
        d3.select("text.playername."+direction)
            .text(name);
        var myidx =    "West,North,East,South".split(/,/g).indexOf(upper),
            previous = "South,West,North,East".split(/,/g)[myidx],
            himnow = (
                    ((game.pc.type == "Playing") && (game.pc.player == upper))
                ||  ((game.pc.type == "Move")    && (game.pc.player == previous))
            );
        if(himnow) {
            d3.select("text.playername."+direction)
                .style("font-weight","bold")
                .style("fill","darkred");
        }
    });
    d3.select("text.playername."+me.color.toLowerCase())
        .style("text-decoration", "underline")
        ;
}

function updatePawns(game) {
    var cells = [].slice.call(document.querySelectorAll("g.tockcell"));
    "west,north,east,south".split(/,/).forEach(function(direction) {
        var upper = direction.slice(0,1).toUpperCase()+direction.slice(1);
        var pawns = game.board[upper] || [];
        var pdl = 2;
        pawns.forEach(function(pawn,j) {
            var id = "pawn"+direction+j;
            var ele = d3.select("#"+id);
            ele.datum(pawn);
            if(pawn.type == "Out") {
                var d = colordeltas[direction];
                var cell = cells[d.base];
                var bb = cell.getBBox();
                var x = bb.x+(pdl+j)*d.dx*42;
                var y = bb.y+(pdl+j)*d.dy*42;
                var cx = x+bb.width/2;
                var cy = y+bb.height/2;
                ele.attr("cx",cx);
                ele.attr("cy",cy);
            } else {
                var pos = pawn.position;
                var cell = cells[pos];
                var bb = cell.getBBox();
                var cx = bb.x+bb.width/2;
                var cy = bb.y+bb.height/2;
                ele.attr("cx",cx);
                ele.attr("cy",cy);
            }
        });
    });
}

function cardfor(card) {
    var v = "Ace,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King"
        .split(/,/).indexOf(card.cardValue)+1;
    var c = { "Spades": "S", "Hearts": "H",
        "Diamonds": "D", "Clubs": "C"}[card.cardColor];
    var result = null;

    var normalv = (v==1)?"A":v;
    try { result = document.getElementById(normalv+c); } catch(err) {}
    if(result) return result;
	var magicid = { "11C":"g4130", "12C":"g4183", "13C":"g4230",
		"11H":"g3966", "12H":"g4021", "13H":"g4069",
		"11S":"g3874", "12S":"g3952", "13S":"g3963",
		"11D":"g3829", "12D":"g3839", "13D":"g3850"}[v+c];
    try { result = document.getElementById(magicid); } catch(err) {}
    if(result) return result;

	return d3.select("#BACK_BLUE1").node();
}

function updateHand(game,me) {
    var svg = d3.select("svg.tockboard").node();

    "Ace,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King".split(
            /,/g).forEach(function(cardValue) {
        "Spades,Hearts,Diamonds,Clubs".split(/,/g).forEach(function(cardColor) {
            var cf = cardfor({cardColor:cardColor,cardValue:cardValue});
            if(cf) {
                cf.style.display = "none";
                d3.select(cf).on("click", null);
            }
        });
    });

    var exchanging = ((game.pc.type == "Exchanging") ||
                      (game.pc.type == "Exchange"));
    d3.selectAll("g.exchange").style("display","none");
    "west,north,east,south".split(/,/).forEach(function(direction) {
        var upper = direction.slice(0,1).toUpperCase()+direction.slice(1);
        d3.select("g.exchange."+direction).style("display",
            (game.players[upper] || {}).hasexchange?"":"none");
    });
    game.hand.forEach(function(c,i) {
        var cf = cardfor(c);
        var g = svg.appendChild(cf.parentNode.removeChild(cf));
        g.style.display = "block";
        g.setAttribute("transform","");
        var bb = g.getBBox();
        g.setAttribute("transform", [
            "translate(",650+i*60," 835) ",
            "scale(0.6) ",
            "translate(",-bb.x," ",-bb.y,")"
            ].join(""));
        d3.select(g).on("click", function(e) {
            var myidx =    "West,North,East,South".split(/,/g).indexOf(me.color),
                previous = "South,West,North,East".split(/,/g)[myidx],
                menow = (
                        ((game.pc.type == "Playing") && (game.pc.player == me.color))
                    ||  ((game.pc.type == "Move")    && (game.pc.player == previous))
                );
            if(exchanging) {
                var sf = ("Card {cardValue = "+c.cardValue
                    +", cardColor = "+c.cardColor+"}");
                d3.request("exchange")
                    .header("Content-Type", "application/x-www-form-urlencoded;charset=utf-8")
                    .post("card="+encodeURIComponent(sf), function(err,unused) {
                        inform("Failed to exchange card", err);
                        console.log("Exchanged: click", c, game);
                    });
            } else if(menow) {
                cardClicked(c,game,me);
            } else {
                alert("NOT YOUR TURN?: clicked "+JSON.stringify(c));
            }
        });
    });

    if(!exchanging) {
        updateLastFour(game);
    }
}

function updateLastFour(game) {
    var svg = d3.select("svg.tockboard").node();
    var moves = game.lastfour.filter(function(h) { return h.type == 'Move'; })
    if(moves.length > 4) { moves = moves.slice(moves.length-4) }
        
    moves.forEach(function(h,i) {
            var cf = cardfor(h.move.card);
            var g = svg.appendChild(cf.parentNode.removeChild(cf));
            g.style.display = "block";
            g.setAttribute("transform","");
            var bb = g.getBBox();
            g.setAttribute("transform", [
                "translate(",320+i*80," 420) ",
                "scale(0.6) ",
                "translate(",-bb.x," ",-bb.y,")"
                ].join(""));
        })
        ;
}

var COLORS = {
    West:"yellow",
    East:"blue",
    North:"red",
    South:"green",
};
function updateWon(game) {
    var ap = Object.keys(game.board).reduce(function(acc,dir) {
        var allParked = !(game.board[dir]
            .find(function(p){return p.type != "Parked"}));
        acc[dir] = allParked;
        return acc;
    },{});
    var won = (ap.West && ap.East) || (ap.North && ap.South);
    if(won) {
        tadaa(COLORS[game.pc.player]);
    }
}

function cardClicked(card,game,me) {
    var matching = function(action) {
            return ((action.card.cardColor == card.cardColor) &&
                    (action.card.cardValue == card.cardValue));
    };
    var remaining = game.actions.filter(matching);
    d3.select("main nav").selectAll("*").remove();
    if(remaining.length == 1) {
        playmove(remaining[0].shown);
    } else {
        var moved = d3.selectAll(".pawn").filter(function(d){
                return ('userMovedTo' in d) && (d.userMovedTo != d.position)
            }),
            l = location.pathname.split(/\//g),
            mycolor = l[l.length-2]
            ;
        console.log("Moves", moved.data());
        function posOrOut(pawn) {
            return (pawn.type == "Out")?"Out":(pawn.position);
        }
        var withpawns = remaining.filter(function(action) {
                if((action.type == "Run") && (moved.size() == 1)) {
                    var p = moved.datum();
                    if((p.position == action.from.position) &&
                       (p.userMovedTo == action.to.position)) {
                        return true;
                    }
                }
                if((action.type == "Seven") && (moved.size() > 0)) {
                    var mine = moved.data()
                        .map(function(d){return [d.position, d.userMovedTo]})
                        .sort();
                    // e.g. 16,21 56,58 58,Out
                    var theirs = action.hops
                        .map(function(hop) {return [posOrOut(hop[0]), posOrOut(hop[1])];})
                        .sort();
                    // e.g. 16,58 56,21 58,Out

                    if(JSON.stringify(mine) == JSON.stringify(theirs)) {
                        return true;
                    } else if(samehops(game,mine,theirs)) {
                        return true;
                    }
                }
                if((action.type == "Enter") && (moved.size() == 1)) {
                    var p = moved.datum();
                    var base = colordeltas[mycolor.toLowerCase()].base;
                    var nonparked = game.board[mycolor].find(
                        function(p){return p.type!="Parked"});
                    if(!nonparked) {
                        var pn = colordeltas[mycolor.toLowerCase()].partner;
                        base = colordeltas[pn].base;
                    }
                    if(p.type == "Out" && (p.userMovedTo == base)) {
                        return true;
                    }
                }
                if((action.type == "Swap") && (moved.size() == 2)) {
                    var p12 = moved.data(), p1=p12[0], p2=p12[1];
                    if((p1.position == p2.userMovedTo) &&
                       (p2.position == p1.userMovedTo)) {
                        var l1 = [p1.position, p2.position].sort();
                        var l2 = [action.source.position, action.target.position].sort();
                        if(JSON.stringify(l1) == JSON.stringify(l2)) {
                            return true;
                        }
                    }
                }
                if((action.type == "Warp") && (moved.size() == 1)) {
                    var p = moved.datum();
                    if((p.position == action.from.position) &&
                       (p.userMovedTo == action.to.position)) {
                        return true;
                    }
                }
            })
            ;
        var byshown = withpawns.reduce(function (acc,d) {
                acc[d.shown]=d;
                return acc;
            },{})
            ;
        var uniques = Object.keys(byshown).map(function(k){return byshown[k]});
        console.log("withpawns", uniques, remaining);

        if(uniques.length == 1) {
            playmove(uniques[0].shown);
        } else {
            var select = d3.select("main nav").append("select");
            var option = select.selectAll("option").data([{shown:"-"}].concat(remaining))
                .enter().append("option");
            option.text(function(d){return d.shown});
            option.attr("value", function(d){return d.shown});
            select.on("change", function() {
                var move = this.value;
                if(move && (move != "-")) {
                    d3.select("main nav").selectAll("*").remove();
                    playmove(move);
                }
            });
        }
    }
}

function samehops(game,mine,theirs) {
    var debug = (
        ('[[16,21],[20,22]]' == JSON.stringify(theirs)) ||
        ('[[16,21],[20,22]]' == JSON.stringify(theirs)) ||
        ('[[16,36],[20,22],[36,80],[80,21]]' == JSON.stringify(theirs))
    );
    var original = Object.keys(game.board)
        .map(function(k){return game.board[k]})
        .reduce(function(a,b){return a.concat(b)},[])
        .filter(function(d){return d.type != "Out"})
        .map(function(d){return d.position})
        .sort()
        ;
    var outMine =   outhops(original.slice(),mine);
    var outTheirs = outhops(original.slice(),theirs);
    return (JSON.stringify(outMine) == JSON.stringify(outTheirs));
}
function outhops(pawns,moves) {
    moves.forEach(function(move){
        var i = pawns.indexOf(move[0]);
        if(i == -1) {
            alert("Bug found! Please contact sebastien@migniot.com "
                +"with the title of your game");
        } else {
            pawns.splice(i,1,move[1]);
        }
    });
    return pawns.sort();
}

function playmove(move) {
    d3.request("play")
        .header("Content-Type", "application/x-www-form-urlencoded;charset=utf-8")
        .post("move="+encodeURIComponent(move), function(err,unused) {
            inform("Failed to play card", err);
            console.log("Played: ", move);
        });
}

/**
 * Tadaa, habemus winna
 * 
 * color: The last player which moved
 */
function tadaa(color) {
    var G = [[1,0],[2,0],[3,0],[0,1],[0,2],[2,2],[3,2],[0,3],
	[3,3],[1,4],[2,4],[3,4]];
    var A = [[1,0],[2,0],[0,1],[3,1],[0,2],[1,2],[2,2],[3,2],[0,3],[0,4],
        [3,3],[3,4]];
    var N = [[1,0],[2,0],[0,1],[3,1],[0,2],[3,2],[0,3],[0,4],[3,3],[3,4]];
    var E = [[1,0],[2,0],[3,0],[0,1],[0,2],[1,2],[2,2],[0,3],[1,4],[2,4]
        ,[3,4]];
    var l = [];
    prepareLetter(G, 0, l);
    prepareLetter(A, 1, l);
    prepareLetter(G, 2, l);
    prepareLetter(N, 3, l);
    prepareLetter(E, 4, l);

    var i,x,y,d,div,l2,x0,y0;
    l2 = [];
    x0 = 350;
    y0 = 350;
    for(i=0; i<l.length; i++) {
        var div = d3.select("body")
            .append("div")
            .attr("class","pawn_"+color)
            .style("left",x0+"px")
            .style("top",y0+"px")
            ;
        x = x0+(l[i][0]-12)*20;
        y = y0+(l[i][1]-2)*20;
        l2.push([div.node(), [x0,y0],[x,y]]);
    }
    for(i=0; i<l2.length; i++) {
        doAnimate(l2[i][0], l2[i][1], l2[i][2], 0);
    }
};

/**
 * Prepare a letter gfx
 *
 * letter: The letter as coordinates
 * offset: The offset in word
 * l: The list to push to
 */
function prepareLetter(letter, offset, l) {
    var i;
    for(i=0; i<letter.length; i++) {
	l.push([letter[i][0]+offset*5, letter[i][1]]);
    }
};

/**
 * Perform animation
 * 
 * div: The pawn div
 * xy0: The original position coordinates
 * xy: The final position coordinates
 * i: The current animation index
 */
var MXMVT = 40;
function doAnimate(div, xy0, xy, i) {
    div.style.left = (xy0[0]+(xy[0]-xy0[0])*i/MXMVT) +'px';
    div.style.top = (xy0[1]+(xy[1]-xy0[1])*i/MXMVT) +'px';
    if(i<MXMVT) {
        setTimeout(function() { doAnimate(div, xy0, xy, i+1); }, 10);
    }
}





