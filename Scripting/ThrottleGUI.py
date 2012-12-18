from breezypythongui import EasyFrame
from thermometer import Thermometer

class ThrottleGUI(EasyFrame):
    """A termperature conversion program."""

    def __init__(self, model):
        """Sets up the view.  The model comes in as an argument."""
        EasyFrame.__init__(self, title="Throttle")
        self.model = model

        # Label and field for train address
        self.addLabel(text="Train address", row=0, column=0)
        self.addressField = self.addIntegerField(value=0, row=0, column=1)

        # Label and field for train position
        self.addLabel(text="Position", row=1, column=0)
        self.positionField = self.addTextField(value=model.getFahrenheit(), row=1, column=1)

        # Label and field for train state
        self.addLabel(text="State", row=2, column=0)
        self.stateField = self.addTextField("[..]", row=2, column=1)

        # Initialize button
        self.addButton(text="Initialize>", row=3, column=0, command=self.initializeTrain, span = 2)

        # Lights button
        self.addButton(text="Lights", row=5, column=0, command=self.changeLight)

    # The controller methods
    def computeFahr(self):
        """Inputs the Celsius degrees
        and outputs the Fahrenheit degrees."""
        degrees = self.celsiusField.getNumber()
        self.model.setCelsius(degrees)
        self.fahrField.setNumber(self.model.getFahrenheit())

    def computeCelsius(self):
        """Inputs the Fahrenheit degrees
        and outputs the Celsius degrees."""
        degrees = self.fahrField.getNumber()
        self.model.setFahrenheit(degrees)
        self.celsiusField.setNumber(self.model.getCelsius())

# Instantiate the model, pass it to the view, and pop up the window.
if __name__ == "__main__":
    model = Thermometer()
    view = ThermometerView(model)
    view.mainloop()


