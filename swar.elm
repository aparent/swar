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

stepPlayers : UserInput -> GameState -> GameState
stepPlayers inp gState = let p1 = gState.player1
                             p1Input = (toFloat inp.x1, toFloat inp.y1, inp.delta)
                             p2 = gState.player2
                             p2Input = (toFloat inp.x2, toFloat inp.y2, inp.delta)
                         in {gState | player1 <- stepPlayerPos inp.delta <| stepPlayerGrav <| stepPlayerInput p1 p1Input
                                    , player2 <- stepPlayerPos inp.delta <| stepPlayerGrav <| stepPlayerInput p2 p2Input} 

stepPlayerInput : Player -> (Float,Float,Float) -> Player
stepPlayerInput p (inpX,inpY,delta) = {p | rot <- p.rot - 5 *inpX*delta
                                         , vx <- p.vx + inpY*(sin p.rot)*delta
                                         , vy <- p.vy - inpY*(cos p.rot)*delta
                                      } 

stepPlayerGrav : Player -> Player
stepPlayerGrav p = let gravAcc = (-1)/(p.x^2 + p.y^2)
                       theta = atan (p.x/p.y)
                   in { p | vx <- clamp (-0.5) 0.5 <| p.vx + 0.001*gravAcc*(p.x/(sqrt(p.x^2+p.y^2)))
                          , vy <- clamp (-0.5) 0.5 <| p.vy + 0.001*gravAcc*(p.y/(sqrt(p.x^2+p.y^2)))
                      }

stepPlayerPos : Float -> Player -> Player
stepPlayerPos delta p = {p | y  <- p.y + p.vy * delta
                           , x <- p.x + p.vx * delta
                        } 


display : (Int,Int) -> GameState -> Element
display (w,h) gState =
  let wf = toFloat w
      hf = toFloat h
      player1 = gState.player1
      player2 = gState.player2
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
