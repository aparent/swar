import Window
import Keyboard

type Player = {x:Float, y:Float, vx:Float, vy:Float, rot:Float, projDelta:Float}
type Projectile = {x:Float, y:Float, vx:Float, vy:Float}
type GameState = {player1:Player,player2:Player,score1:Int,score2:Int,projectiles:[Projectile]}

delta = inSeconds <~ fps 35

shotDelay = 0.5

type UserInput = {x1:Int, y1:Int, x2:Int, y2:Int, delta:Float} 

input : Signal UserInput
input = sampleOn delta (UserInput <~ lift .x Keyboard.wasd 
                                   ~ lift .y Keyboard.wasd
                                   ~ lift .x Keyboard.arrows
                                   ~ lift .y Keyboard.arrows
                                   ~ delta) 

player1Start = {x=-0.5,y=0,vx=0,vy=0,rot=0,projDelta=0}
player2Start = {x=0.5,y=0,vx=0,vy=0,rot=0 ,projDelta=0}
defaultGame = {player1 = player1Start, player2 = player2Start, score1 = 0, score2 = 0, projectiles =[]}

stepGame : UserInput -> GameState -> GameState
stepGame inp gState = checkBounds <| stepProjectiles inp.delta <| fireProjectiles inp <| stepPlayers inp gState

stepProjectiles : Float -> GameState -> GameState
stepProjectiles delta gs = let gravAcc p = (-1)/(p.x^2 + p.y^2)
                               stepProjectile p = { p | vx <- p.vx + 0.001*(gravAcc p)*(p.x/(sqrt(p.x^2+p.y^2)))
                                                , vy <- p.vy + 0.001*(gravAcc p)*(p.y/(sqrt(p.x^2+p.y^2)))
                                                , x <-  p.x + p.vx*delta 
                                                , y <-  p.y + p.vy*delta
                                            }         
                               dist p  = sqrt(p.x^2 + p.y^2) 
                           in { gs | projectiles <- filter (\p -> dist p < 1 && dist p > 0.1) <| map stepProjectile gs.projectiles }

fireProjectiles : UserInput -> GameState -> GameState
fireProjectiles inp gameState= let fireProj p = { x = p.x + 0.03 * sin p.rot, y = p.y - 0.03 * cos p.rot, vx = p.vx + sin p.rot , vy = p.vy - cos p.rot}
                                   resetProjDelta p = {p | projDelta <- 0} 
                                   fireP1 gs = if gs.player1.projDelta > shotDelay
                                               then {gs | player1 <- resetProjDelta gs.player1 
                                                        , projectiles <- fireProj gs.player1 :: gs.projectiles} 
                                               else gs 
                                   fireP2 gs = if gs.player2.projDelta > shotDelay
                                               then {gs | player2 <- resetProjDelta gs.player2
                                                        , projectiles <- fireProj gs.player2 :: gs.projectiles} 
                                               else gs 
                                in (fireP2 . fireP1) gameState

checkBounds : GameState -> GameState
checkBounds gState = let dist p      = sqrt(p.x^2 + p.y^2) 
                         sep a b = sqrt((a.x-b.x)^2 + (a.y-b.y)^2)  
                         boundsP1 gs = if (dist gs.player1 > 1 || dist gs.player1 < 0.1 || any (\x -> sep gs.player1 x < 0.02) gs.projectiles )
                                       then {gs | player1 <- player1Start, score2 <- gs.score2 + 1}
                                       else gs
                         boundsP2 gs = if (dist gs.player2 > 1|| dist gs.player2 < 0.1 || any (\x -> sep gs.player2 x < 0.02) gs.projectiles)
                                       then {gs | player2 <- player2Start, score1 <- gs.score1 + 1}
                                       else gs
                     in (boundsP2 . boundsP1) gState

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
                                         , projDelta <- p.projDelta + delta 
                                      } 

stepPlayerGrav : Player -> Player
stepPlayerGrav p = let gravAcc = (-1)/(p.x^2 + p.y^2)
                   in { p | vx <- clamp (-0.5) 0.5 <| p.vx + 0.001*gravAcc*(p.x/(sqrt(p.x^2+p.y^2)))
                          , vy <- clamp (-0.5) 0.5 <| p.vy + 0.001*gravAcc*(p.y/(sqrt(p.x^2+p.y^2)))
                      }

stepPlayerPos : Float -> Player -> Player
stepPlayerPos delta p = {p | y  <- p.y + p.vy * delta
                           , x  <- p.x + p.vx * delta
                        } 


display : (Int,Int) -> GameState -> Element
display (w,h) gState =
  let wf = toFloat w
      hf = toFloat h
      player1 = gState.player1
      player2 = gState.player2
      projs =  map (drawProjectile gameRad) gState.projectiles 
      gameRad = min wf hf / 2
      gameBoundry = circle (gameRad) 
                    |> filled black 
      star        = circle (gameRad/10) 
                    |> filled red 
      drawPlayerScaled = drawPlayer <| gameRad
      p1Image = drawPlayerScaled yellow player1
      p2Image = drawPlayerScaled green player2
      s1Image = move (-9*gameRad/10, 9*gameRad/10)<| toForm . text . Text.height 60 . Text.color yellow . toText . show <| gState.score1 
      s2Image = move ( 9*gameRad/10, 9*gameRad/10)<| toForm . text . Text.height 60 . Text.color green  . toText . show <| gState.score2
  in collage w h ([gameBoundry, star, p1Image, p2Image, s1Image, s2Image ] ++ projs)

drawProjectile : Float -> Projectile -> Form
drawProjectile winScale p = let xT = winScale*p.x
                                yT = winScale*p.y
                            in circle (winScale/100)
                               |> filled white
                               |> move (xT,yT)



drawPlayer : Float -> Color ->  Player -> Form
drawPlayer winScale c p = let xT = winScale*p.x
                              yT = winScale*p.y
                          in polygon [(0,-winScale/20),(winScale/80,0),(-winScale/80,0)]
                            |> filled c
                            |> rotate p.rot 
                            |> move (xT,yT)

gameState = foldp stepGame defaultGame input


main = lift2 display Window.dimensions gameState
