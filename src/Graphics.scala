/**
 * Created by isaacbanner on 7/11/14.
 */

import swing._
import event._

object Graphics extends SimpleSwingApplication{
  def top = new MainFrame {
    title = "Window Title"
    val btn1 = new Button{
      text = "Button Text (So META!)"
    }
    val lbl1 = new Label{
      text = "This is a Label taking about Labels"
    }
    object celsius extends TextField { columns = 5 }
    object fahrenheit extends TextField { columns = 5 }

    val fp = new FlowPanel {
      contents += celsius
      contents += new Label("Celsius  = ")
      contents += fahrenheit
      contents += new Label("Fahrenheit")
      border = Swing.EmptyBorder(10, 10, 10, 10)
    }

    contents = new BoxPanel(Orientation.Vertical) {
      contents += btn1
      contents += lbl1
      contents += fp
      border = Swing.EmptyBorder(30, 30, 30, 30)
    }

    listenTo(btn1)
    listenTo(celsius, fahrenheit)
    reactions += {
      case ButtonClicked(b) => {
        lbl1.text = "This is a Label talking about \nhow you pressed a Button!"
      }
      case EditDone(`fahrenheit`) => {
        val f = fahrenheit.text.toInt
        val c = (f - 32) * 5 / 9
        celsius.text = c.toString
      }
      case EditDone(`celsius`) => {
        val c = celsius.text.toInt
        val f = c * 9 / 5 + 32
        fahrenheit.text = f.toString
      }
    }
  }
}
