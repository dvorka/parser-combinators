//---------------------------------------------------------------------------
//
//			   Part of JNaaga game
//                                Shot
//
//                             Martin Dvorak
//				   1999
//---------------------------------------------------------------------------

// package JNaaga;

import java.applet.*;
import java.awt.*;
import java.awt.image.*;
import java.net.*;
import java.util.*;

class Shot extends Thread
{
    public static final int WIDTH=2,          // size
                            HEIGHT=2;
    public int              x1,x2,
                            y1,y2,
                            tired,
                            minYCache,
                            step,              // move speed
                            antiEnergy,        // decreases energy of enemies
                            i;             
    private JNaaga          m;
    private Enemy           e;   
    public  Graphics        g;                 // for speedup
    private Vector          inFight;

    public Shot( int x, int y, int tired, int step, JNaaga m, Graphics g,
                 int antiEnergy )
    {
        this.tired=tired;
        this.step=step;
        x1=x;
        x2=x+WIDTH;
        y1=y;
        y2=y+HEIGHT;
        this.m=m;
        this.g=g;
        minYCache=JNaaga.BOARD_MIN_Y-HEIGHT;
        this.inFight=m.inFight;
        this.antiEnergy=antiEnergy;
        
        this.start();
    }

    

    public void run()
    {
        while( y1>minYCache )
        {
         synchronized( m.virtualScreen )
         {
             // delete old things which stays after shot
             g.fillRect(x1,y1,WIDTH,HEIGHT);
             // new positon (changes should be done atomicly)
             y1-=step; y2-=step;
             // draw image...
             g.drawImage( m.images[JNaaga.SHOT], x1, y1, m );
         }

         // test collision with enemy
         for( i=0; i<inFight.size(); i++ )
         {
             e=(Enemy)inFight.elementAt(i);
             
             // test collsion
             if( ((x2-e.x1)*(x1-e.x2)<=0) && ((y2-e.y1)*(y1-e.y2)<=0) )
             {
                 // decrease energy of enemy
                 e.energy-=antiEnergy;
                 if( e.energy>0 )       // enemy still has some energy
                 {
                     // only suicide, enemy lives but shot dies
                     synchronized( m.virtualScreen ) { g.fillRect(x1,y1,WIDTH,HEIGHT); }
                     this.stop();
                 }
                 // else shot kills the enemy and both dies

                 e.goToDock(); // monster killed -> say it 

                 synchronized( m.virtualScreen )
                 {
                     g.drawString(""+m.gameScore,273,125);
                     g.drawString(""+m.gameKilled,276,97);
                     if( m.percent>=100 )
                         g.drawString(""+m.percent,288,40);
                     else
                         g.drawString(""+m.percent+"%",288,40);
                     m.gameKilled++;
                     m.levelKilled++;
                     m.gameScore+=e.price; // add enemy price
                     // write score
                     g.setColor(m.scoreColor);
                     g.drawString(""+m.gameScore,273,125);
                     // percent killed
                     m.percent=(int)((((float)m.gameKilled)/((float)((m.level-1)*m.MAX_LEVEL_ENEMY+m.levelEnemy)))*(float)100.0); // max je 5
                     if( m.percent>100 ) m.percent=100;
                     if( m.percent>=100 )
                         g.drawString(""+m.percent,288,40); // draw percentage
                     else
                         g.drawString(""+m.percent+"%",288,40); // draw percentage
                     // and global killed
                     g.drawString(""+m.gameKilled,276,97);
                     // draw level killed enemies meter
                     g.setColor(m.killedColor);
                     g.drawLine(307,135+m.levelKilled,315,135+m.levelKilled);
                     // set black
                     g.setColor(Color.black);
                     // clear after shot
                     g.fillRect(x1,y1,WIDTH,HEIGHT);
                 }

                 this.stop();
             }
         } // for ... testing collision

         // give time others to draw
         Thread.yield();
        }

        // end of screen -> delete old things which stays after shuttle
        synchronized( m.virtualScreen )
        {
            g.fillRect(x1,y1,WIDTH,HEIGHT);
        }

        this.stop();
    }

} // class Shot
