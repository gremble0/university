import javax.swing.*;

class Kjoer implements Runnable {
    private boolean fortsett = true;
    private Snake snake;
    private long hastighet = 2000;

    public Kjoer(Snake snake) {
        this.snake = snake;
    }

    @Override
    public void run() {
        try {
            while (fortsett) {
                Thread.sleep(hastighet);
                JLabel hodeRef = snake.hentHodeRef();
                snake.flytt(hodeRef);
                if (hastighet > 100) {
                    hastighet -= 100;
                }
            }
        } catch (InterruptedException e) {
            System.exit(1);
        } catch (IndexOutOfBoundsException e) { // hvis vi proever aa flytte oss mens vi er paa kanten av brettet
            fortsett = false;
        } catch (NullPointerException e) {
            fortsett = false;
        }
    }
}