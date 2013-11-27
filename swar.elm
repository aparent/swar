import Window
import Keyboard

type Player = {x:Float, y:Float, vx:Float, vy:Float, rot:Float}
type GameState = {player1:Player,player2:Player}

delta = inSeconds <~ fps 35

type UserInput = {x1:Int, y1:Int, x2:Int, y2:Int, delta:Float} 

input : Signal UserInput
input = sampleOn delta (UserInput <~ lift .x Keyboard.wasd 
                                   ~ lift .y Keyboard.wasd
                                   ~ lift .x Keyboard.arrows
                                   ~ lift .y Keyboard.arrows
                                   ~ delta) 

defaultGame = {player1 = {x=-0.5,y=0,vx=0,vy=0,rot=0}, player2 = {x=0.5,y=0,vx=0,vy=0,rot=0} }

stepGame : UserInput -> GameState -> GameState
stepGame inp gState = stepPlayers inp gState


--{gState | player1 <- stepPlayer gState.player1 (toFloat inp.x1, toFloat inp.y1, inp.delta)
--                              , player2 <- stepPlayer gState.player2 (toFloat inp.x2, toFloat inp.y2, inp.delta)} 

stepPlayers : UserInput -> GameState -> GameState
stepPlayers inp gState = {gState | player1 <- stepPlayerInput gState.player1 (toFloat inp.x1, toFloat inp.y1, inp.delta)
                                 , player2 <- stepPlayerInput gState.player2 (toFloat inp.x2, toFloat inp.y2, inp.delta)} 

stepPlayerInput : Player -> (Float,Float,Float) -> Player
stepPlayerInput p (inpX,inpY,delta) = {p | rot <- p.rot - 5 *inpX*delta
                                    , vx <- clamp (-0.5) 0.5 (p.vx + inpY*(sin p.rot)*delta)
                                    , vy <- clamp (-0.5) 0.5 (p.vy - inpY*(cos p.rot)*delta)
                                    ,  y <- if abs (p.y + p.vy*delta) > abs (cos <| abs p.x) 
                                           then -p.y 
                                           else p.y + p.vy * delta
                                    ,  x <- if abs (p.x + p.vx * delta) > abs (cos <| abs p.y) 
                                           then -p.x 
                                           else p.x + p.vx * delta
                                 } 

display : (Int,Int) -> GameState -> Element
display (w,h) ({player1,player2} as gameState) =
  let wf = toFloat w
      hf = toFloat h
      gameRad = min wf hf / 2
      gameBoundry = circle (gameRad) 
                    |> filled black 
      drawPlayerScaled = drawPlayer <| min wf hf / 2
      p1Image = drawPlayerScaled yellow player1
      p2Image = drawPlayerScaled green player2
  in collage w h [gameBoundry, p1Image, p2Image ]

drawPlayer : Float -> Color ->  Player -> Form
drawPlayer winScale c p = let xT = winScale*p.x
                              yT = winScale*p.y
                          in polygon [(0,-winScale/20),(winScale/80,0),(-winScale/80,0)]
                            |> filled c
                            |> rotate p.rot 
                            |> move (xT,yT)

gameState = foldp stepGame defaultGame input


main = lift2 display Window.dimensions gameState
