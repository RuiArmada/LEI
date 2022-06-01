package Model.Interfaces;

import java.util.Date;

import Model.Review;

public interface Review_Interface {
    
    public Review clone();
    public String toString();
    public boolean equals(Object obj);
    public String getBusiness_id();
    public String get_user_id();
    public int getCool();
    public Date getDate();
    public int getFunny();
    public float getStars();
    public String getText();
    public int getUseful();
    public void setBusiness_id(String business_id);
    public void setCool(int cool);
    public void setDate(Date date);
    public void setFunny(int funny);
    public void set_user_id(String user_id);
    public void setStars(float stars);
    public void setText(String text);
    public void setUseful(int useful);

}
