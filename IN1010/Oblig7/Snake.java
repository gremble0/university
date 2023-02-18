import java.awt.*;
import java.awt.event.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Queue;
import javax.swing.*;

public class Snake {
    private String retning = "hoyre";
    private int hodePos = 76;
    private Queue<JLabel> slange = new LinkedList<>();
    private JFrame vindu = new JFrame("Snake - hermagst");
    private JPanel main = new JPanel(new GridBagLayout());
    private JPanel spillPanel = new JPanel(new GridLayout(12, 12, -1, -1)); 

    public Snake() {
        try {
            UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName());
        } catch (Exception e) {
            System.exit(1);
        }
        vindu.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        gbc.fill = GridBagConstraints.BOTH;

        JPanel header = new JPanel(new GridBagLayout());

        // ------ KNAPPER FOR AA ENDRE RETNING ------

        class FlyttRetning implements ActionListener {
            String retning;
            public FlyttRetning(String retning) {
                this.retning = retning;
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                endreRetning(retning);
            }
        }
        
        class FlyttRetningPiltast implements KeyListener {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_UP) {
                    endreRetning("opp");
                } else if (e.getKeyCode() == KeyEvent.VK_RIGHT) {
                    endreRetning("hoyre");
                } else if (e.getKeyCode() == KeyEvent.VK_DOWN) {
                    endreRetning("ned");
                } else if (e.getKeyCode() == KeyEvent.VK_LEFT) {
                    endreRetning("venstre");
                }
            }

            @Override
            public void keyReleased(KeyEvent e) {}

            @Override
            public void keyTyped(KeyEvent e) {}
        }

        JButton oppKnapp = new JButton("Opp");
        gbc.gridx = 1;
        header.add(oppKnapp, gbc);
        oppKnapp.addActionListener(new FlyttRetning("opp"));

        JButton venstreKnapp = new JButton("Venstre");
        gbc.gridx = 0; gbc.gridy = 1;
        header.add(venstreKnapp, gbc);
        venstreKnapp.addActionListener(new FlyttRetning("venstre"));

        JButton hoyreKnapp = new JButton("Hoyre");
        gbc.gridx = 2;
        header.add(hoyreKnapp, gbc);
        hoyreKnapp.addActionListener(new FlyttRetning("hoyre"));

        JButton nedKnapp = new JButton("Ned");
        gbc.gridx = 1; gbc.gridy = 2;
        header.add(nedKnapp, gbc);
        nedKnapp.addActionListener(new FlyttRetning("ned"));
        

        // ------ INITIERING AV RUTENETT ------

        ArrayList<Integer> skatter = new ArrayList<>(); // liste av indekser der det skal vaere skatter

        for (int i = 0; i < 10; i++) { // i < 10 for aa starte med 10 skatter
            int tilfeldigInt = (int)Math.floor(Math.random()*(143-0+1)+0); // 144 ruter aa velge mellom siden 12*12

            if (skatter.contains(tilfeldigInt) || tilfeldigInt == 76) {
                i -= 1;
                continue; // hvis det tilfeldige tallet allerede har blitt generert, eller tallet er der hvor slangen skal starte (boks 6,6), lag et nytt tall
            }
            skatter.add(tilfeldigInt);
        }

        for (int i = 0; i < 144; i++) {
            JLabel boks;

            if (skatter.contains(i)) { // hvis vi er paa en boks der det er generert en skatt...
                boks = new JLabel("$");
                boks.setForeground(Color.red);
                boks.setBackground(Color.white);
            } else if (i == 76) { // der slangen skal starte
                boks = new JLabel("o");
                boks.setBackground(Color.green);
                slange.add(boks);
            } else {
                boks = new JLabel(" ");
                boks.setBackground(Color.white);
            }

            boks.setOpaque(true);
            boks.setFont(new Font("Arial", Font.BOLD, 20));
            boks.setHorizontalAlignment(SwingConstants.CENTER);
            boks.setBorder(BorderFactory.createLineBorder(Color.black)); // lager en svart ramme rundt hver boks i griden
            spillPanel.add(boks);
        }

        // ------ VISING AV GUI ------

        gbc.gridy = 0;
        header.setPreferredSize(new Dimension(500,100));
        main.add(header, gbc);
        gbc.gridy = 1;
        spillPanel.setPreferredSize(new Dimension(500,500));
        main.add(spillPanel, gbc);

        vindu.add(main);
        vindu.pack();
        vindu.setVisible(true);

        vindu.setFocusable(true);
        vindu.addKeyListener(new FlyttRetningPiltast());
    }

    public void endreRetning(String nyRetning) {
        retning = nyRetning;
    }

    public void flytt(JLabel hode) {
        JLabel nesteBoks;

        if (retning == "opp") {
            if (hodePos <= 11) { 
                spillPanel = null; // fant ingen bedre maate aa stoppe programmet uten System.exit(), hvilket umiddelbart lukker guien
                return;
            }
            hodePos -= 12;
            nesteBoks = (JLabel) spillPanel.getComponent(hodePos);
        } else if (retning == "hoyre") {
            if ((hodePos + 1) % 12 == 0) {
                spillPanel = null;
                return;
            }
            hodePos += 1;
            nesteBoks = (JLabel) spillPanel.getComponent(hodePos);
        } else if (retning == "ned") {
            if (hodePos >= 132) { // hvis hodePos er stoerre enn eller lik foerste rute paa nederste rad...
                spillPanel = null;
                return;
            }
            hodePos += 12;
            nesteBoks = (JLabel) spillPanel.getComponent(hodePos);
        } else {
            if (hodePos % 12 == 0) {
                spillPanel = null;
                return;
            }
            hodePos -= 1;
            nesteBoks = (JLabel) spillPanel.getComponent(hodePos);
        }

        if (nesteBoks.getText() == "+") {
            spillPanel = null;
            return;
        }

        if (nesteBoks.getText().equals("$")) { // hvis neste boks har en skatt...
            slange.offer(nesteBoks);
            for (JLabel slangeBoks : slange) {
                tegnIBoks(slangeBoks, "+", Color.green, Color.black);
            }
            tegnIBoks(nesteBoks, "o", Color.green, Color.black);

            int tilfeldigInt = (int)Math.floor(Math.random()*(143-0+1)+0);
            JLabel boks = (JLabel)spillPanel.getComponent(tilfeldigInt);
            while (boks.getText() == "o" || boks.getText() == "+" || boks.getText() == "$") { // repeter til generert boks ikke er en del av slangen eller allerede en skatt
                // vil gaa i uendelig loekke hvis slangen blir veldig lang, men ikke et reelt problem
                tilfeldigInt = (int)Math.floor(Math.random()*(143-0+1)+0);
                boks = (JLabel)spillPanel.getComponent(tilfeldigInt);
            }

            tegnIBoks(boks, "$", Color.white, Color.red);
            return;
        } 

        slange.offer(nesteBoks);
        for (JLabel slangeBoks : slange) {
            tegnIBoks(slangeBoks, "+", Color.green, Color.black);
        }

        tegnIBoks(nesteBoks, "o", Color.green, Color.black);
        tegnIBoks(slange.remove(), "", Color.white, Color.white);
    }

    public void tegnIBoks(JLabel boks, String text, Color bakgrunn, Color forgrunn) {
        boks.setText(text);
        boks.setBackground(bakgrunn);
        boks.setForeground(forgrunn);
    }

    public JLabel hentHodeRef() {
        return slange.peek();
    }
}