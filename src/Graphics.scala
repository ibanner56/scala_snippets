/**
 * Created by isaacbanner on 7/11/14.
 */

import scala.swing._
import scala.swing.event.ButtonClicked

object Graphics extends SimpleSwingApplication{
  def top = new MainFrame {
    title = "Window Title"
    val btn1 = new Button{
      text = "Button Text (So META!)"
    }
    val lbl1 = new Label{
      text = "This is a Label taking about Labels"
    }
    contents = new BoxPanel(Orientation.Vertical) {
      contents += btn1
      contents += lbl1
      border = Swing.EmptyBorder(30, 30, 30, 30)
    }
    listenTo(btn1)
    reactions += {
      case ButtonClicked(b) => {
        lbl1.text = "This is a Label talking about \nhow you pressed a Button!"
      }
    }
  }
}
